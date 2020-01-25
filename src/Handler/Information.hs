module Handler.Information
    ( information
    , serverError
    ) where

import           Prelude                   hiding (userError)

import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (Status)

import qualified Handler.Templates         as T
import           Handler.Utils

-- |Display an informational dialogue
information :: Text -- ^ The message to display
            -> Handler db
information = htmlResponse . T.notice

-- |Display a scary red dialogue with a status code
serverError :: Status -- ^ The status code
            -> Text   -- ^ The friendly message
            -> Handler db
serverError status = htmlResponse' status . T.noticeError
