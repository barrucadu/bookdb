{-# LANGUAGE OverloadedStrings #-}

module Handler.Utils
    ( onReadWrite
    , withBook
    , with
    , unEntities
    , unValues) where

import Prelude hiding (userError)

import Data.Text (Text)
import Database.Persist
import Database.Esqueleto (Value(..))

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
  readonly <- conf "bookdb" "readonly"

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

-- | Combination of 'when', 'isJust', and 'fromJust'.
with :: Monad m => Maybe a -> (a -> m ()) -> m ()
with (Just a) f = f a
with Nothing _  = pure ()

-- | Turn a @[Entity a]@ into a @[a]@.
unEntities :: [Entity a] -> [a]
unEntities = map (\(Entity _ a) -> a)

-- | Turn a @[Value a]@ into a @[a]@.
unValues :: [Value a] -> [a]
unValues = map (\(Value a) -> a)
