module Handler.Information
    ( information
    , userError
    , serverError
    ) where

import           Prelude                   hiding (userError)

import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (Status)

import           Requests
import           Routes

import qualified Handler.Templates         as T

-- |Display an informational dialogue
information :: Text -- ^ The message to display
            -> Handler Sitemap
information = htmlUrlResponse . T.notice

-- |Display a scary red dialogue
userError :: Text -- ^ The message to berate them with
          -> Handler Sitemap
userError = htmlUrlResponse . T.noticeError

-- |Display a scary red dialogue with a status code
serverError :: Status -- ^ The status code
            -> Text   -- ^ The friendly message
            -> Handler Sitemap
serverError status = htmlUrlResponse' status . T.noticeError
