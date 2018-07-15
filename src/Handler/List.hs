{-# LANGUAGE OverloadedStrings #-}

module Handler.List
    ( list
    , search
    , restrict
    ) where

import Control.Monad.Trans.Class (lift)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import Data.Text (unpack, toLower)
import Database.Selda (Col, Cols)
import Database.Selda.Generic (Relation)

import Database
import Routes
import Requests

import qualified Handler.Templates as T

list :: Handler Sitemap
list = do
  books <- sortBooks <$> lift allBooks

  htmlUrlResponse $ T.index books

search :: Handler Sitemap
search = do
  isbn        <- fromMaybe "" <$> param "isbn"
  title       <- fromMaybe "" <$> param "title"
  subtitle    <- fromMaybe "" <$> param "subtitle"
  author      <- fromMaybe "" <$> param "author"
  matchread   <- hasParam "matchread"
  matchunread <- hasParam "matchunread"
  location    <- fromMaybe "" <$> param "location"
  category    <- fromMaybe Uncategorised <$> ((>>= categoryOf) <$> param "category")
  borrower    <- fromMaybe "" <$> param "borrower"

  books <- sortBooks <$> lift (searchBooks isbn title subtitle author location borrower category matchread matchunread)

  htmlUrlResponse $ T.search isbn title subtitle author matchread matchunread location borrower category books

-- |Filter by field value
restrict :: (Cols s (Relation Book) -> Col s Bool) -- ^ The filter
         -> Handler Sitemap
restrict by = do
  books <- sortBooks <$> lift (restrictBooks by)

  htmlUrlResponse $ T.index books

-- | Sort a book list.
sortBooks :: [Book] -> [Book]
sortBooks = sortBy cmp where
  cmp = (Down . bookNowReading) <>: key <>: bookTitle <>: (split . bookVolume) <>: comparing (split . bookFascicle)
  key book = toLower . fromMaybe (bookAuthor book) $ bookSorting book

  split txt = case span isDigit $ unpack txt of
    (digits, str) -> (read ('0':digits) :: Int, str)

  infixr <>:
  cmp1 <>: cmp2 = \a b -> comparing cmp1 a b <> cmp2 a b
