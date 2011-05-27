{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances
  , MultiParamTypeClasses, UndecidableInstances, FlexibleContexts
  , TypeSynonymInstances #-}

module Network.UWeb where

import Data.List            (head, tail, foldl', null, map)
import Data.Char            (digitToInt, chr)
import Data.Text            (Text(..), pack)
import Data.Text.Encoding   (encodeUtf8)
import Data.Convertible     (Convertible, safeConvert)
import Data.CaseInsensitive (mk)
import Data.Enumerator.List (consume)
import Network.Wai          (Request(..), Application, responseLBS)

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LS
import qualified Network.HTTP.Types  as HTTP


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
            | Status HTTP.Status
            deriving (Eq, Show)


----------------
-- WAI RUNNER --
----------------

-- | The main function of the framework. Takes a web application which
--   returns something which can be converted to a ResponseBody, and
--   lifts it up into a proper runnable WAI `Application` value.
uWeb :: Convertible a ResponseBody => AppT (Request, BS.ByteString) IO a -> Application
uWeb app req = liftIO . dispatch app req =<< BS.concat <$> consume
  where dispatch app req body = do
            response <- runAppT app (req, body)
            return $ case response of
                Left failure -> do
                    case safeConvert failure of
                        Left err -> responseLBS HTTP.status500 [] errFail
                        Right cr -> responseLBS HTTP.status500 [] cr
                Right (b, h) ->
                    case safeConvert b of
                        Left err -> responseLBS HTTP.status500 [] errSucc
                        Right cr -> responseLBS (convStat h) (convHdrs h) cr
        errFail = "request failed -- unable to render response"
        errSucc = "request succeeded -- unable to render response"

-- | Comb through a list of headers and find the HTTP status code
convStat = foldl' nextStat HTTP.statusOK
  where nextStat stat (Header _ _) = stat
        nextStat stat (Status new) = new

-- | Comb through a list of headers and remove any HTTP status codes
--   then convert them to the correct Network.HTTP `Header` type.
convHdrs = Prelude.map convHdr . Prelude.filter notStat
  where convHdr (Header name val) = (mk $ encodeUtf8 name, encodeUtf8 val)
        notStat (Header _ _) = True
        notStat (Status  _ ) = False


-----------------------------------------
-- CONTENT GENERATION HELPER FUNCTIONS --
-----------------------------------------

-- | A helper function which sets an HTTP header field.
--   Calling it multiple times with the same field name
--   will result in multiple header entries for that field.
httpHeader :: Monad m => Text -> Text -> AppT (Request, BS.ByteString) m ()
httpHeader key val = tell [Header key val]

-- | A helper function which sets the HTTP status code.
--   If called multiple times the last invocation will take
--   precedence over all the others.
httpStatus :: Monad m => HTTP.Status -> AppT (Request, BS.ByteString) m ()
httpStatus code = tell [Status code]

-- | A helper function which throws a page generation error.
pageError :: Monad m => Text -> AppT (Request, BS.ByteString) m ()
pageError err = throwError (Failure err)


----------------------------------------
-- REQUEST DATA INTERACTION FUNCTIONS --
----------------------------------------

-- | A typeclass for request types which provide a list of path elements
class WebRequestPath r where
    readRequestPath :: r -> [Text]
    editRequestPath :: ([Text] -> [Text]) -> r -> r

-- | The top-level application request type provides path elements
instance WebRequestPath (Request, BS.ByteString) where
    readRequestPath = pathInfo . fst
    editRequestPath f (w, b) = (w { pathInfo = f (pathInfo w) } , b)


-------------------------
-- ROUTING COMBINATORS --
-------------------------

-- | A helper function which checks that its first argument is the
--   head element of the `pathInfo` list, and then executes its second
--   argument in an environment with that element stripped off the list.
--   Throws a page generation error if the list is empty or the head
--   doesn't match the expected value.
url s a = do
    req <- ask
    let p = readRequestPath req
    guard $ not (null p)
    guard $ head p == s
    local (\_ -> editRequestPath tail req) a

-- | Does the opposite of the `url` helper function, and errors out unless
--   all elements of the `pathInfo` list have been consumed. Essentially
--   matches against the URL fragment "/".
leafPage a = do
    req <- ask
    guard . null . readRequestPath $ req
    a
