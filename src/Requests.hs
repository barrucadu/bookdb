{-# LANGUAGE OverloadedStrings #-}

-- |Building up responses. This module provides a bunch of functions
-- to turn some primitive value into a handler, and the child modules
-- provide more complex handler composition.
--
-- In addition to response building, there are helper functions for
-- accessing the request parameters.
module Requests
    ( -- * Types
      module Requests.Types

    -- * Response builders
    , htmlResponse
    , htmlResponse'

    , htmlUrlResponse
    , htmlUrlResponse'

    , textResponse
    , textResponse'

    , textUrlResponse
    , textUrlResponse'

    , respond
    , respondFile

    -- * Redirection
    , redirect

    -- * Parameter accessors
    , param
    , param'
    , hasParam
    , params

    -- * File upload handling
    , files
    , save
    , save') where

import Prelude hiding (writeFile)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (unpack)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Char (chr)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status (Status, ok200, found302)
import Network.Mime (defaultMimeLookup)
import Network.Wai (responseBuilder, responseFile, responseLBS)
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath, takeExtension, takeFileName)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import Configuration
import Requests.Types

-- |Produce a 200 OK response from the given HTML.
htmlResponse :: Html -> Handler r
htmlResponse = htmlResponse' ok200

-- |Produce a response from the given HTML and response code.
htmlResponse' :: Status -> Html -> Handler r
htmlResponse' status html = respond status $ renderHtmlBuilder html

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

-- |Produce a 200 OK response from the given UTF-8 text.
textResponse :: Text -> Handler r
textResponse = textResponse' ok200

-- |Produce a response from the given UTF-8 text and response
-- code.
textResponse' :: Status -> Text -> Handler r
textResponse' status = respond status . fromByteString . encodeUtf8

-------------------------

-- |Produce a 200 OK response from the given UTF-8 text-generating
-- function.
textUrlResponse :: (MkUrl r -> Text) -> Handler r
textUrlResponse = textUrlResponse' ok200

-- |Produce a response from the given UTF-8 text-generating function
-- and response code.
textUrlResponse' :: Status -> (MkUrl r -> Text) -> Handler r
textUrlResponse' status text = do
  mkurl <- askMkUrl
  respond status . fromByteString . encodeUtf8 $ text mkurl

-------------------------

-- |Produce a response from the given status and ByteString
-- builder. This sets a content-type of UTF-8 HTML.
respond :: Status -> Builder -> Handler r
respond status = return . responseBuilder status [("Content-Type", "text/html; charset=utf-8")]

-- |Produce a response from the given file path (minus file
-- root). Call the provided 404 handler if the file isn't found.
respondFile :: Handler r -> FilePath -> Handler r
respondFile on404 fp = do
  fileroot <- conf' "bookdb" "file_root"
  let fullPath = joinPath [fileroot, fp]

  respondFile' on404 fullPath

-- |Produce a response from the given file path (including any
-- root). Call the provided 404 handler if the file isn't found.
--
-- This is somewhat unsafe as it lets you access files outside the
-- file root, hence why it isn't exported.
respondFile' :: Handler r -> FilePath -> Handler r
respondFile' on404 fp = do
  exists <- liftIO $ doesFileExist fp
  if exists
  then return $ responseFile ok200 [("Content-Type", defaultMimeLookup $ pack fp)] fp Nothing
  else on404

-------------------------

-- |Produce a response to redirect the user.
redirect :: r -> Handler r
redirect url = do
  mkurl <- askMkUrl
  return $ responseLBS found302 [("Location", encodeUtf8 $ mkurl url [])] ""

-------------------------

-- |Get a parameter by name. Returns a Maybe Text, where the Text is
-- the value of the parameter, interpreted as a UTF-8 string.
param :: PathInfo r
      => Text -- ^ The name of the parameter
      -> RequestProcessor r (Maybe Text)
param p = lookup p <$> params

-- |Get a parameter with a default value.
param' :: PathInfo r
       => Text -- ^ The parameter name
       -> Text -- ^ The default value
       -> RequestProcessor r Text
param' p d = fromMaybe d <$> lookup p <$> params

-- |Check if a parameter is set
hasParam :: PathInfo r
         => Text -- ^ The parameter name
         -> RequestProcessor r Bool
hasParam p = isJust <$> lookup p <$> params

-- |Get all non-file parameters, with the contents interpreted as
-- UTF-8 strings.
params :: RequestProcessor r [(Text, Text)]
params = _params <$> askCry

-------------------------

-- |Get all files, stored in memory as a lazy bytestring.
files :: RequestProcessor r [(Text, FileInfo ByteString)]
files = _files <$> askCry

-- |Save a file to a location relative to the filesystem root,
-- returning the name. File extension is preserved.
save :: FilePath -> FileInfo ByteString -> RequestProcessor r Text
save fname (FileInfo name _ content) = do
  fileroot <- conf' "bookdb" "file_root"
  let ext    = takeExtension (map (chr . fromIntegral) $ unpack name)
  let path   = joinPath [fileroot, fname]
  let fname' = path ++ ext

  liftIO $ writeFile fname' content

  return . pack $ takeFileName fname'

-- |Save a file to a location relative to the filesystem root, with
-- the path given as segments, returning the name. File extension is
-- preserved.
save' :: [FilePath] -> FileInfo ByteString -> RequestProcessor r Text
save' = save . joinPath
