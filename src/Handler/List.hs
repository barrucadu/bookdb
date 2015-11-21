{-# LANGUAGE OverloadedStrings #-}

module Handler.List
    ( index
    , search
    , restrict
    , restrictFuzzy
    ) where

import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text, unpack)
import Database.Esqueleto hiding ((==.))
import Database.Persist hiding ((||.))

import Database
import Handler.Utils
import Routes
import Requests

import qualified Database.Esqueleto as E
import qualified Handler.Templates as T

index :: Handler Sitemap
index = do
  books <- sortBooks <$> selectList [] []

  htmlUrlResponse $ T.index books

search :: Handler Sitemap
search = do
  isbn        <- param "isbn"
  title       <- param "title"
  subtitle    <- param "subtitle"
  author      <- param "author"
  matchread   <- hasParam "matchread"
  matchunread <- hasParam "matchunread"
  location    <- param "location"
  category    <- (>>= categoryOf) <$> param "category"
  borrower    <- param "borrower"

  books <- fmap sortBooks . select $
          from $ \b -> do
            with isbn $ \isbn ->
              where_ (b ^. BookIsbn     `like` (%) ++. val isbn ++. (%))
            with title $ \title ->
              where_ (b ^. BookTitle    `like` (%) ++. val title ++. (%))
            with subtitle $ \subtitle ->
              where_ (b ^. BookSubtitle `like` (%) ++. val subtitle ++. (%))
            with author $ \author ->
              where_ (b ^. BookAuthor   `like` (%) ++. val author ++. (%))
            with location $ \location ->
              where_ (b ^. BookLocation `like` (%) ++. val location ++. (%))
            with borrower $ \borrower ->
              where_ (b ^. BookBorrower `like` (%) ++. val borrower ++. (%))
            with category $ \category ->
              where_ (b ^. BookCategory E.==. val category)
            where_ ((b ^. BookRead &&. val matchread) ||. (not_ (b ^. BookRead) &&. val matchunread))
            return b

  htmlUrlResponse $ T.search isbn title subtitle author matchread matchunread location borrower category books

-- |Filter by exact field value
restrict :: PersistField t
         => EntityField Book t -- ^ The field to filter on
         -> t -- ^ the value to filter by
         -> Handler Sitemap
restrict field is = do
  books <- sortBooks <$> selectList [ field ==. is ] []

  htmlUrlResponse $ T.index books

-- |Filter by fuzzy field value
restrictFuzzy :: EntityField Book Text -- ^ The field to filter on
              -> Text -- ^ The value to filter by
              -> Handler Sitemap
restrictFuzzy field contains = do
  books <- fmap sortBooks . select $
          from $ \b -> do
            where_ (b ^. field `like` (%) ++. val contains ++. (%))
            return b

  htmlUrlResponse $ T.index books

-- | Sort a book list.
sortBooks :: [Entity Book] -> [Book]
sortBooks = sortBy cmp . map (\(Entity _ b) -> b) where
  cmp a b = comparing key a b <> comparing bookTitle a b <> comparing (split . bookVolume) a b <> comparing (split . bookFascicle) a b
  key book = fromMaybe (bookAuthor book) $ bookSorting book
  split txt = case span isDigit $ unpack txt of
    (digits, str) -> (read ('0':digits) :: Int, str)