{-# LANGUAGE OverloadedStrings #-}

module Handler.Utils
    ( suggest
    , onReadWrite
    , withBook) where

import Prelude hiding (userError)

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database
import Database.Persist
import Handler.Information (userError)
import Routes
import System.Random (randomRIO)
import Web.Seacat

-- |Get a suggestion
suggest :: RequestProcessor Sitemap (Maybe Book)
suggest = do
  books <- selectList [BookRead ==. False] []

  idx <- liftIO $ randomRIO (0, length books - 1)

  if null books
  then return Nothing
  else return . Just . (\(Entity _ e) -> e) $ books !! idx


-- |Run the given handler if in read-write mode, otherwise display an
-- error page.
onReadWrite :: Handler Sitemap -- ^ The handler
            -> Handler Sitemap
onReadWrite handler = do
  readonly <- conf' "server" "readonly"

  if readonly
  then userError "Database is read-only"
  else handler

-- |Run a handler which tskes a book as an argument, identified by
-- ISBN, and display an error if there is no such book.
withBook :: (Entity Book -> Handler Sitemap) -- ^ The handler
         -> Text -- ^ The ISBN
         -> Handler Sitemap
withBook handler isbn = do
  book <- selectFirst [BookIsbn ==. isbn] []
  case book of
    Just b  -> handler b
    Nothing -> userError "No such book"
