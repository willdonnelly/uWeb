{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}

module Network.UWeb.Core where

import Data.List                (foldl')
import Data.Char                (digitToInt, chr)
import Data.Text                (Text(..), pack)
import Data.Text.Encoding       (encodeUtf8)
import Data.Convertible         (Convertible, safeConvert)
import Data.CaseInsensitive     (mk)
import Data.Enumerator.List     (consume)
import Network.Wai              (Request(..), Application, responseLBS)
import Network.HTTP.Types       (Status, status500, statusOK)

import Control.Applicative      ((<$>))
import Control.Monad            (MonadPlus)
import Control.Monad.Trans      (MonadIO, liftIO)
import Control.Monad.Reader     (ReaderT, MonadReader, runReaderT, withReaderT)
import Control.Monad.Writer     (WriterT, MonadWriter, runWriterT)
import Control.Monad.Error      (ErrorT, MonadError, runErrorT, Error(..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LS


----------------------
-- CORE MONAD STACK --
----------------------

-- | This is the core monad stack for uWeb.
--   The `Reader` transformer is used to provide access to the HTTP request.
--   The `Writer` transformer is used to collect HTTP headers and status code
--     (which defaults to status200 if unset).
--   The `Error` transformer is used to provide filtering behaviour when used
--     in conjunction with the MonadPlus instance.
newtype AppT r m a = AppT {
    unAppT :: ReaderT r (WriterT [Header] (ErrorT Failure m)) a
  } deriving ( Monad, MonadPlus, Functor, MonadIO
             , MonadReader r
             , MonadWriter [Header]
             , MonadError Failure )

-- | Run a given web application with the provided request, and returns
--   either `m (Left error)` or `m (Right (body, headers))`
runAppT :: AppT r m a -> r -> m (Either Failure (a, [Header]))
runAppT app req = runErrorT . runWriterT . (flip runReaderT req) . unAppT $ app


-------------------------------------------
-- HANDY CONTENT DATATYPES AND INSTANCES --
-------------------------------------------

-- | WAI uses a lazy ByteString for the response body, so this type
--   synonym exists to make Convertible instances prettier
type ResponseBody = LS.ByteString

-- | The Failure type is used to represent page generation errors.
--   It can be ignored in favor of the `pageError` helper function.
data Failure = Failure Text deriving (Eq, Show)

-- | We have to declare this instance so that Failure can be the
--   error type for the ErrorT monad transformer.
instance Error Failure where
    noMsg  = Failure "Generic Error!"
    strMsg = Failure . pack

-- | We need to be able to output failure messages as a response,
--   so they need to be convertible to a ResponseBody
instance Convertible Failure ResponseBody where
    safeConvert (Failure e) = Right . LS.fromChunks . (:[]) . encodeUtf8 $ e

-- | The Header type is used to store HTTP headers and the HTTP status
--   code, as both of these can be set during page generation through
--   the WriterT monad transformer.
data Header = Header Text Text
            | Status Status
            deriving (Eq, Show)


----------------
-- WAI RUNNER --
----------------

-- | The main function of the framework. Takes a web application which
--   returns something which can be converted to a ResponseBody, and
--   lifts it up into a proper runnable WAI `Application` value.
uWeb :: Convertible a ResponseBody => AppT (Request, FormData) IO a -> Application
uWeb app req = liftIO . dispatch (withFormData app) req =<< BS.concat <$> consume
  where dispatch app req body = do
            response <- runAppT app (req, body)
            return $ case response of
                Left failure -> do
                    case safeConvert failure of
                        Left err -> responseLBS status500 [] errFail
                        Right cr -> responseLBS status500 [] cr
                Right (b, h) ->
                    case safeConvert b of
                        Left err -> responseLBS status500 [] errSucc
                        Right cr -> responseLBS (convStat h) (convHdrs h) cr
        errFail = "request failed -- unable to render response"
        errSucc = "request succeeded -- unable to render response"

-- | Comb through a list of headers and find the HTTP status code
convStat = foldl' nextStat statusOK
  where nextStat stat (Header _ _) = stat
        nextStat stat (Status new) = new

-- | Comb through a list of headers and remove any HTTP status codes
--   then convert them to the correct Network.HTTP `Header` type.
convHdrs = Prelude.map convHdr . Prelude.filter notStat
  where convHdr (Header name val) = (mk $ encodeUtf8 name, encodeUtf8 val)
        notStat (Header _ _) = True
        notStat (Status  _ ) = False


----------------------------------
-- FORM DATA PARSING MIDDLEWARE --
----------------------------------

data FormData = URLEncoded [(Text, Text)]
              | MultiPart  [(Text, Text, BS.ByteString)]
              deriving (Eq, Show)

withFormData :: AppT (Request, FormData) m a -> AppT (Request, BS.ByteString) m a
withFormData = AppT . withReaderT (\(r,s) -> (r, parseFormData r s)) . unAppT

parseFormData :: Request -> BS.ByteString -> FormData
parseFormData req
  | ct == Just "multipart/form-data" = MultiPart  . parseMP
  | otherwise                        = URLEncoded . parseKV
  where ct = "content-type" `lookup` requestHeaders req

-- | A helper function which decodes url-encoded HTTP requests.
parseKV :: BS.ByteString -> [(Text, Text)]
parseKV = map (decodeKV . BS.break (== '=')) . BS.split '&'
  where decodeKV (a, b) = (decode a, decode (BS.tail b))
        decode = pack . url . BS.unpack
        url ('+':xs) = ' ' : url xs
        url ('%':x:y:zs) = chr (16 * digitToInt x + digitToInt y) : url zs
        url (x:xs) = x : url xs
        url [] = []

parseMP :: BS.ByteString -> [(Text, Text, BS.ByteString)]
parseMP s = [("", "", s)]
