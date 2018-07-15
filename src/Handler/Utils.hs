{-# LANGUAGE OverloadedStrings #-}

module Handler.Utils where

import Prelude hiding (userError)

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)

import Configuration
import Database
import Handler.Information
import Routes
import Requests

-- |Run the given handler if in read-write mode, otherwise display an
-- error page.
onReadWrite :: Handler Sitemap -- ^ The handler
            -> Handler Sitemap
onReadWrite handler = do
  readonly <- conf "readonly"

  if readonly
  then userError "Database is read-only"
  else handler

-- |Run a handler which tskes a book as an argument, identified by
-- ISBN, and display an error if there is no such book.
withBook :: (Book -> Handler Sitemap) -- ^ The handler
         -> Text -- ^ The ISBN
         -> Handler Sitemap
withBook handler isbn = do
  book <- lift $ findBook isbn
  case book of
    Just b  -> handler b
    Nothing -> userError "No such book"

-- | Combination of 'when', 'isJust', and 'fromJust'.
with :: Monad m => Maybe a -> (a -> m ()) -> m ()
with (Just a) f = f a
with Nothing _  = pure ()
