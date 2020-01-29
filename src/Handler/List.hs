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
  isbn        <- paramWithDefault "isbn" ""
  title       <- paramWithDefault "title" ""
  subtitle    <- paramWithDefault "subtitle" ""
  author      <- paramWithDefault "author" ""
  matchread   <- hasParam "matchread"
  matchunread <- hasParam "matchunread"
  location    <- paramWithDefault "location" ""
  code        <- paramWithDefault "category" ""
  borrower    <- paramWithDefault "borrower" ""

  categories <- lift (lift allCategories)
  let category = categoryByCode' code categories
  books <- sortBooks <$> lift (lift (searchBooks isbn title subtitle author location borrower category matchread matchunread))

  htmlResponse $ T.search categories isbn title subtitle author matchread matchunread location borrower category books

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
