{-# LANGUAGE OverloadedStrings #-}

module Handler.List
    ( index
    , search
    , restrict
    , restrictFuzzy
    ) where

import Control.Monad (when)
import Data.List (sortBy)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import Database.Esqueleto hiding ((==.))
import Database.Persist hiding ((||.))

import Database
import Handler.Utils
import Routes
import Requests

import qualified Handler.Templates as T

index :: Handler Sitemap
index = do
  suggestion <- suggest
  books <- selectList [] []

  let books' = sortBooks $ map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.index suggestion books'

search :: Handler Sitemap
search = do
  suggestion <- suggest

  isbn        <- param "isbn"
  title       <- param "title"
  subtitle    <- param "subtitle"
  author      <- param "author"
  matchread   <- hasParam "matchread"
  matchunread <- hasParam "matchunread"
  location    <- param "location"
  borrower    <- param "borrower"

  books <- select $
          from $ \b -> do
            when (isJust isbn) $
              where_ (b ^. BookIsbn     `like` (%) ++. val (fromJust isbn) ++. (%))
            when (isJust title) $
              where_ (b ^. BookTitle    `like` (%) ++. val (fromJust title) ++. (%))
            when (isJust subtitle) $
              where_ (b ^. BookSubtitle `like` (%) ++. val (fromJust title) ++. (%))
            when (isJust author) $
              where_ (b ^. BookAuthor   `like` (%) ++. val (fromJust author) ++. (%))
            when (isJust location) $
              where_ (b ^. BookLocation `like` (%) ++. val (fromJust location) ++. (%))
            when (isJust borrower) $
              where_ (b ^. BookBorrower `like` (%) ++. val (fromJust borrower) ++. (%))
            where_ ((b ^. BookRead &&. val matchread) ||. (not_ (b ^. BookRead) &&. val matchunread))
            return b

  let books' = sortBooks $ map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.search suggestion isbn title subtitle author matchread matchunread location borrower books'

-- |Filter by exact field value
restrict :: PersistField t
         => EntityField Book t -- ^ The field to filter on
         -> t -- ^ the value to filter by
         -> Handler Sitemap
restrict field is = do
  suggestion <- suggest
  books <- selectList [ field ==. is ] []

  let books' = sortBooks $ map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.index suggestion books'

-- |Filter by fuzzy field value
restrictFuzzy :: EntityField Book Text -- ^ The field to filter on
              -> Text -- ^ The value to filter by
              -> Handler Sitemap
restrictFuzzy field contains = do
  suggestion <- suggest

  books <- select $
          from $ \b -> do
            where_ (b ^. field `like` (%) ++. val contains ++. (%))
            return b

  let books' = sortBooks $ map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.index suggestion books'

-- | Sort a book list.
sortBooks :: [Book] -> [Book]
sortBooks = sortBy cmp where
  cmp a b = comparing key a b <> comparing bookTitle a b <> comparing bookVolume a b <> comparing bookFascicle a b
  key book = fromMaybe (bookAuthor book) $ bookSorting book
