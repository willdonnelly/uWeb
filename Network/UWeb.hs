{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances
  , MultiParamTypeClasses, UndecidableInstances, FlexibleContexts
  , TypeSynonymInstances #-}

module Network.UWeb where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Data.List
import Data.Char (digitToInt, chr)
import Data.Text hiding (head, tail, foldl', null, map)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Convertible
import Data.CaseInsensitive hiding (map)

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Enumerator.List as ELST

-- | WAI uses a lazy ByteString for the response body, so this type
--   synonym simply exists to make Convertible instances prettier
type ResponseBody = LBS.ByteString

-- | A request contains all data relevant to a given HTTP request,
--   including (unlike WAI's Request type) parsed form data.
data Request = Request { waiRequest :: WAI.Request
                       , formData :: [(Text, Text)] }

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
    safeConvert (Failure e) = Right . LBS.fromChunks . (:[]) . encodeUtf8 $ e

-- | The Header type is used to store HTTP headers and the status
--   code, as both of these can be set during page generation using
--   the WriterT monad transformer.
data Header  = Header Text Text
             | Status HTTP.Status
             deriving (Eq, Show)

-- | This is the core monad stack for uWeb. The Reader transformer is used
--   to provide access to the current HTTP request, the Writer transformer
--   is used to collect HTTP headers and optionally a status code (which
--   defaults to HTTP 200 if unset), and the Error transformer is used in
--   combination with the MonadPlus instance to provide choice of when a
--   given page generator can be used.
newtype AppT m a = AppT {
    unAppT :: ReaderT Request (WriterT [Header] (ErrorT Failure m)) a
  } deriving ( Monad, MonadPlus, Functor, MonadIO
             , MonadReader Request
             , MonadWriter [Header]
             , MonadError Failure )

-- | Actual web applications will generally need to run in the IO monad
--   in order to get useful work like database interaction done.
type App = AppT IO

-- | Run a given web application with the provided request, and returns
--   either `m (Left error)` or `m (Right (body, headers))`
runAppT :: AppT m a -> Request -> m (Either Failure (a, [Header]))
runAppT app req = runErrorT . runWriterT . (flip runReaderT req) . unAppT $ app

-- | The main function of the framework. Takes a web application which
--   returns something which can be converted to a ResponseBody, and
--   lifts it up into a proper runnable WAI `Application` value.
uWeb :: Convertible a ResponseBody => AppT IO a -> WAI.Application
uWeb app req = liftIO . dispatch app req =<< BS.concat <$> ELST.consume
  where dispatch app req body = do
            response <- runAppT app (Request req $ parseBody req body)
            case response of
                Left failure -> do
                    return $ WAI.responseLBS HTTP.status500 [] (convert failure)
                Right (body, hdrs) -> do
                    return $ WAI.responseLBS (convStat hdrs) (convHdrs hdrs) (convert body)
        convStat = foldl' combStat HTTP.statusOK
          where combStat stat (Header _ _) = stat
                combStat stat (Status new) = new
        convHdrs = Prelude.map convHdr . Prelude.filter notStat
          where convHdr (Header name val) = (mk $ encodeUtf8 name, encodeUtf8 val)
                notStat (Header _ _) = True
                notStat (Status  _ ) = False

-- | A helper function which takes the HTTP request body and parses
--   it into fields depending on the Content-Type header.
parseBody :: WAI.Request -> BS.ByteString -> [(Text, Text)]
parseBody req body
  | ct == Just "application/x-www-form-urlencoded" = parseKV body
  | otherwise = []
  where ct = "content-type" `lookup` WAI.requestHeaders req

-- | A helper function which decodes url-encoded HTTP requests.
parseKV :: BS.ByteString -> [(Text, Text)]
parseKV = map (decodeKV . BS.break (== '=')) . BS.split '&'
  where decodeKV (a, b) = (decode a, decode (BS.tail b))
        decode = pack . url . BS.unpack
        url ('+':xs) = ' ' : url xs
        url ('%':x:y:zs) = chr (16 * digitToInt x + digitToInt y) : url zs
        url (x:xs) = x : url xs
        url [] = []

-- | A helper function which sets an HTTP header field.
--   Calling it multiple times with the same field name
--   will result in multiple header entries for that field.
httpHeader :: Monad m => Text -> Text -> AppT m ()
httpHeader key val = tell [Header key val]

-- | A helper function which sets the HTTP status code.
--   If called multiple times the last invocation will take
--   precedence over all the others.
httpStatus :: Monad m => HTTP.Status -> AppT m ()
httpStatus code = tell [Status code]

-- | A helper function which throws a page generation error.
pageError :: Monad m => Text -> AppT m ()
pageError err = throwError (Failure err)

-- | A helper function which returns the WAI `pathInfo` list.
requestPagePath :: (Monad m, Functor m) => AppT m [Text]
requestPagePath = WAI.pathInfo . waiRequest <$> ask

-- | A helper function which returns the parsed form data.
requestFormData :: (Monad m, Functor m) => AppT m [(Text, Text)]
requestFormData = formData <$> ask

-- | A helper function which checks that its first argument is the
--   head element of the `pathInfo` list, and then executes its second
--   argument in an environment with that element stripped off the list.
--   Throws a page generation error if the list is empty or the head
--   doesn't match the expected value.
url s a = do
    r <- ask
    let p = WAI.pathInfo (waiRequest r)
    guard $ not (null p)
    guard $ head p == s
    let n = Request ((waiRequest r) { WAI.pathInfo = tail p }) (formData r)
    local (\_ -> n) a

-- | Does the opposite of the `url` helper function, and errors out unless
--   all elements of the `pathInfo` list have been consumed. Essentially
--   matches against the URL fragment "/".
leafPage a = do
    r <- ask
    guard . null . WAI.pathInfo . waiRequest $ r
    a
