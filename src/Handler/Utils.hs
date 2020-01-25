{-# LANGUAGE OverloadedStrings #-}

module Handler.Utils
  ( Handler
  , module Handler.Utils
  ) where

import           Prelude                       hiding (userError)

import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Reader    (ask)
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL
import           Network.HTTP.Types.Status     (Status (..), ok200)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty.Trans              as S

import           Configuration
import           Database
import qualified Handler.Templates             as T

-- |Run the given handler if in read-write mode, otherwise display an
-- error page.
onReadWrite :: Handler db -- ^ The handler
            -> Handler db
onReadWrite handler = do
  readonly <- cfgReadOnly <$> lift ask

  if readonly
  then userError "Database is read-only"
  else handler

-- |Run a handler which tskes a book as an argument, identified by
-- ISBN, and display an error if there is no such book.
withBook :: (Book -> Handler db) -- ^ The handler
         -> Text -- ^ The ISBN
         -> Handler db
withBook handler isbn = do
  book <- lift . lift $ findBook isbn
  case book of
    Just b  -> handler b
    Nothing -> userError "No such book"

-- |Display a scary red dialogue
userError :: Text -- ^ The message to berate them with
          -> Handler db
userError = htmlResponse . T.noticeError

-- | Combination of 'when', 'isJust', and 'fromJust'.
with :: Monad m => Maybe a -> (a -> m ()) -> m ()
with (Just a) f = f a
with Nothing _  = pure ()

-- |Produce a 200 OK response.
htmlResponse :: (Text -> Html) -> Handler db
htmlResponse = htmlResponse' ok200

-- |Produce a response.
htmlResponse' :: Status -> (Text -> Html) -> Handler db
htmlResponse' status html = do
  web_root <- cfgWebRoot <$> lift ask
  S.status status
  S.html . renderHtml $ html web_root

-- |Get a parameter with a default value.
paramWithDefault :: Monad m
                 => TL.Text -- ^ The parameter name
                 -> Text -- ^ The default value
                 -> RequestProcessor db m Text
paramWithDefault p d = S.param p `S.rescue` const (pure d)

-- |Check if a parameter is set
hasParam :: Monad m
         => TL.Text -- ^ The parameter name
         -> RequestProcessor db m Bool
hasParam p = ((const True :: Text -> Bool) <$> S.param p) `S.rescue` const (pure False)
