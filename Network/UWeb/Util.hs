{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Network.UWeb.Util where

import Data.Text                (Text(..))
import Data.Text.Encoding       (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString.Char8    (ByteString)
import Network.Wai              (Request(..))
import Network.HTTP.Types       (Status)

import Control.Applicative      ((<$>))
import Control.Monad            (MonadPlus)
import Control.Monad.Reader     (MonadReader(..))
import Control.Monad.Writer     (MonadWriter(..))
import Control.Monad.Error      (MonadError(..), guard)

import Network.UWeb.Core


-----------------------------------------
-- CONTENT GENERATION HELPER FUNCTIONS --
-----------------------------------------

-- | A helper function which sets an HTTP header field.
--   Calling it multiple times with the same field name
--   will result in multiple header entries for that field.
httpHeader :: Monad m => Text -> Text -> AppT r m ()
httpHeader key val = tell [Header key val]

-- | A helper function which sets the HTTP status code.
--   If called multiple times the last invocation will take
--   precedence over all the others.
httpStatus :: Monad m => Status -> AppT r m ()
httpStatus code = tell [Status code]

-- | A helper function which throws a page generation error.
pageError :: Monad m => Text -> AppT r m ()
pageError err = throwError (Failure err)


----------------------------------------
-- REQUEST DATA INTERACTION FUNCTIONS --
----------------------------------------

-- | A typeclass for request types which provide a list of path elements
class WebRequestPath r where
    readRequestPath :: r -> [Text]
    editRequestPath :: ([Text] -> [Text]) -> r -> r

-- | A typeclass for request types which provide submitted form data
class WebRequestForm r where
    readRequestForm :: r -> FormData
    editRequestForm :: (FormData -> FormData) -> r -> r

-- | The top-level application request type provides path elements
instance WebRequestPath (Request, FormData) where
    readRequestPath = pathInfo . fst
    editRequestPath f (r, d) = (r { pathInfo = f (pathInfo r) } , d)

-- | The top-level application request type provides form data
instance WebRequestForm (Request, FormData) where
    readRequestForm = snd
    editRequestForm f (r, d) = (r , f d)

-- | Get a list of key-value pairs from the form data
formDataKV :: (WebRequestForm r, MonadReader r m, Functor m, MonadPlus m) => m [(Text, Text)]
formDataKV = form' . readRequestForm <$> ask
  where form' (URLEncoded ue) = ue
        form' (MultiPart  mp) = map unwrap mp
        unwrap (key, ct, val) = (key, decodeUtf8With lenientDecode val)

-- | Get a list of key-ContentType-value triplets from the form data
formDataMP :: (WebRequestForm r, MonadReader r m, Functor m, MonadPlus m) => m [(Text, Text, ByteString)]
formDataMP = form' . readRequestForm <$> ask
  where form' (URLEncoded ue) = map wrap ue
        form' (MultiPart  mp) = mp
        wrap (key, val)       = (key, "text/plain", encodeUtf8 val)

-------------------------
-- ROUTING COMBINATORS --
-------------------------

-- | A helper function which checks that its first argument is the
--   head element of the `pathInfo` list, and then executes its second
--   argument in an environment with that element stripped off the list.
--   Throws a page generation error if the list is empty or the head
--   doesn't match the expected value.
url :: (WebRequestPath r, MonadReader r m, MonadPlus m) => Text -> m b -> m b
url s a = do
    req <- ask
    guard . not    . null . readRequestPath $ req
    guard . (== s) . head . readRequestPath $ req
    local (\_ -> editRequestPath tail req) a

-- | Does the opposite of the `url` helper function, and errors out unless
--   all elements of the `pathInfo` list have been consumed. Essentially
--   matches against the URL fragment "/".
leaf :: (WebRequestPath a, MonadReader a m, MonadPlus m) => m b -> m b
leaf a = ask >>= \req -> (guard . null . readRequestPath $ req) >> a
