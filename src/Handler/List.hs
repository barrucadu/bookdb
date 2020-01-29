{-# LANGUAGE OverloadedStrings #-}

module Handler.List
    ( search
    , restrict
    ) where

import           Control.Monad.Trans.Class (lift)
import           Data.Char                 (isDigit)
import           Data.List                 (sortBy)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (comparing)
import           Data.Text                 (toLower, unpack)
import           Database.Selda            (Col, Row)

import           Database

import qualified Handler.Templates         as T
import           Handler.Utils

search :: Handler db
search = do
  title     <- paramWithDefault "title" ""
  author    <- paramWithDefault "author" ""
  matchcode <- paramWithDefault "match" "all"
  location  <- paramWithDefault "location" ""
  code      <- paramWithDefault "category" ""
  borrower  <- paramWithDefault "borrower" ""

  categories <- lift (lift allCategories)
  locations  <- lift (lift allLocations)
  borrowers  <- lift (lift allBorrowers)
  let category = categoryByCode' code categories
  let match = case matchcode of "only-read" -> Just True; "only-unread" -> Just False; _ -> Nothing
  books <- sortBooks <$> lift (lift (searchBooks title author location borrower category match))

  htmlResponse $ T.search categories locations borrowers title author matchcode location borrower category books

-- |Filter by field value
restrict :: (Row db Book -> Col db Bool) -- ^ The filter
         -> Handler db
restrict by = do
  categories <- lift (lift allCategories)
  books <- sortBooks <$> lift (lift (restrictBooks by))

  htmlResponse $ T.index categories books

-- | Sort a book list.
sortBooks :: [Book] -> [Book]
sortBooks = sortBy cmp where
  cmp = key <>: bookTitle <>: (split . bookVolume) <>: comparing (split . bookFascicle)
  key book = toLower . fromMaybe (bookAuthor book) $ bookSorting book

  split txt = case span isDigit $ unpack txt of
    (digits, str) -> (read ('0':digits) :: Int, str)

  infixr <>:
  cmp1 <>: cmp2 = \a b -> comparing cmp1 a b <> cmp2 a b
