{-# LANGUAGE OverloadedStrings #-}

-- |Building up responses.
module Requests
    ( -- * Types
      Request(..)
    , MkUrl
    , RequestProcessor
    , Handler
    , FileInfo(..)
    , PathInfo(..)

    -- * RequestHandler accessors
    , askConf
    , askMkUrl
    , askParams
    , askFiles

    -- * Response builders
    , htmlUrlResponse
    , htmlUrlResponse'

    -- * Parameter accessors
    , param
    , param'
    , hasParam) where

import           Blaze.ByteString.Builder      (Builder)
import           Control.Monad.Trans.Reader    (ReaderT, ask)
import           Data.ByteString.Lazy          (ByteString)
import           Data.Maybe                    (fromMaybe, isJust)
import           Data.Text                     (Text)
import           Database.Selda                (SeldaM)
import           Network.HTTP.Types.Status     (Status (..), ok200)
import           Network.Wai                   (Response, responseBuilder)
import           Network.Wai.Parse             (FileInfo (..))
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Web.Routes.PathInfo           (PathInfo (..))

import           Configuration                 (Configuration)

-- |Type to represent a Seacat request
data Request r = Request
    { _params :: [(Text, Text)]
    -- ^ The parameters, parsed once before the top-level handler is
    -- called.

    , _files  :: [(Text, FileInfo ByteString)]
    -- ^ The files, stored in memory as lazy bytestrings, and parsed
    -- out of the request once at the beginning.

    , _conf   :: Configuration
    -- ^ The contents of the configuration file

    , _mkurl  :: MkUrl r
    -- ^ The URL building function
    }

-- |Function to make URLs from some routing type
type MkUrl r = r -> [(Text, Text)] -> Text

-- |Function which handles a request
type RequestProcessor r = ReaderT (Request r) SeldaM

-- |`RequestProcessor` specialised to producing a `Response`. All
-- routes should go to a function of type `PathInfo r => Handler r`.
type Handler r = RequestProcessor r Response

-------------------------

-- |Get the configuration from a `RequestProcessor`
askConf :: RequestProcessor r Configuration
askConf = _conf <$> request

-- |Get the URL maker from a `RequestProcessor`
askMkUrl :: RequestProcessor r (MkUrl r)
askMkUrl = _mkurl <$> request

-- |Get the parameter list from a 'RequestProcessor'
askParams :: RequestProcessor r [(Text, Text)]
askParams = _params <$> request

-- |Get the file list from a 'RequestProcessor'
askFiles :: RequestProcessor r [(Text, FileInfo ByteString)]
askFiles = _files <$> request

-- |Get the request from a `RequestProcessor`
request :: RequestProcessor r (Request r)
request = ask

-------------------------

-- |Produce a 200 OK response from the given HTML-generating
-- function.
htmlUrlResponse :: (MkUrl r -> Html) -> Handler r
htmlUrlResponse = htmlUrlResponse' ok200

-- |Produce a response from the given HTML-generating function and
-- response code.
htmlUrlResponse' :: Status -> (MkUrl r -> Html) -> Handler r
htmlUrlResponse' status html = do
  mkurl <- askMkUrl
  let builder = renderHtmlBuilder $ html mkurl
  respond status builder

-------------------------

-- |Produce a response from the given status and ByteString
-- builder. This sets a content-type of UTF-8 HTML.
respond :: Status -> Builder -> Handler r
respond status =
  pure . responseBuilder status [("Content-Type", "text/html; charset=utf-8")]

-------------------------

-- |Get a parameter by name. Returns a Maybe Text, where the Text is
-- the value of the parameter, interpreted as a UTF-8 string.
param :: PathInfo r
      => Text -- ^ The name of the parameter
      -> RequestProcessor r (Maybe Text)
param p = lookup p <$> askParams

-- |Get a parameter with a default value.
param' :: PathInfo r
       => Text -- ^ The parameter name
       -> Text -- ^ The default value
       -> RequestProcessor r Text
param' p d = fromMaybe d . lookup p <$> askParams

-- |Check if a parameter is set
hasParam :: PathInfo r
         => Text -- ^ The parameter name
         -> RequestProcessor r Bool
hasParam p = isJust . lookup p <$> askParams
