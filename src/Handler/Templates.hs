{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Handler.Templates
    ( -- * Modifying books
      addForm
    , editForm
    , confirmDelete

    -- * Displaying information
    , notice
    , noticeError

    -- * Browsing books
    , search
    , index
    ) where

import Prelude hiding (null)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text, splitOn, null, toLower)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Hamlet (HtmlUrl, hamletFile)

import Database
import Routes

import qualified Data.Set as S

-------------------------

index :: Maybe Book -- ^ A suggestion
      -> [Book]     -- ^ The list of all books
      -> HtmlUrl Sitemap
index suggestion books =
  let title   = "BookDB" :: Text
      body    = list books (numAuthors books) (numRead books)
  in $(hamletFile "templates/wrapper.hamlet")

search :: Maybe Book -- ^ A suggestion
       -> Maybe Text -- ^ The ISBN
       -> Maybe Text -- ^ The title
       -> Maybe Text -- ^ The subtitle
       -> Maybe Text -- ^ The author
       -> Bool -- ^ Whether to match read
       -> Bool -- ^ Whether to match unread
       -> Maybe Text -- ^ The location
       -> Maybe Text -- ^ The borrower
       -> Maybe BookCategory -- ^ The category
       -> [Book] -- ^ Books matching the search
       -> HtmlUrl Sitemap
search suggestion isbn btitle subtitle author matchread matchunread location borrower category books = 
  let authors = numAuthors books
      read    = numRead books
      title   = "BookDB :: Search" :: Text
      body    = $(hamletFile "templates/search.hamlet")
  in $(hamletFile "templates/wrapper.hamlet")

list :: [Book] -- ^ The books
     -> Int    -- ^ The number of authors
     -> Int    -- ^ The number of read books
     -> HtmlUrl Sitemap
list books authors read = $(hamletFile "templates/list.hamlet")

-------------------------

addForm :: Maybe Book -- ^ A suggestion
        -> HtmlUrl Sitemap
addForm suggestion =
  let book   = Nothing
      target = Add
      title  = "BookDB :: Add" :: Text
      body   = $(hamletFile "templates/edit_form.hamlet")
  in $(hamletFile "templates/wrapper.hamlet")

editForm :: Maybe Book -- ^ A suggestion
         -> Book -- ^ The book to edit
         -> HtmlUrl Sitemap
editForm suggestion bk =
  let book   = Just bk
      target = Edit $ bookIsbn bk
      title  = "BookDB :: Edit " <> bookIsbn bk :: Text
      body   = $(hamletFile "templates/edit_form.hamlet")
  in $(hamletFile "templates/wrapper.hamlet")

confirmDelete :: Book -- ^ The book to delete
              -> HtmlUrl Sitemap
confirmDelete book = $(hamletFile "templates/confirm_delete.hamlet")

-------------------------

notice :: Text -- ^ The message
       -> HtmlUrl Sitemap
notice message = let error = False
                 in $(hamletFile "templates/information.hamlet")

noticeError :: Text -- ^ The message
            -> HtmlUrl Sitemap
noticeError message = let error = True
                      in $(hamletFile "templates/information.hamlet")

-------------------------

-- |Get the number of unique authors in a list of books
numAuthors :: [Book] -> Int
numAuthors = S.size . authors

-- |Get the list of unique authors in a list of books
authors :: [Book] -> Set Text
authors = foldr authors' S.empty
  where authors' b s = S.union s . S.fromList . splitOn " & " $ bookAuthor b

-- |Get the number of read books in a list of books
numRead :: [Book] -> Int
numRead = length . filter bookRead

-- |Pluralise a word
pluralise :: Text -- ^ The singular
          -> Text -- ^ The plural
          -> Int -- ^ The count
          -> Text
pluralise s _ 1 = s
pluralise _ p _ = p

-- |Calculate a percentage
percent :: Int -- ^ The actual value
        -> Int -- ^ The max value
        -> Int
percent _ 0 = 100
percent a b = round $ (fromIntegral a / fromIntegral b) * 100

-- |Pretty-print a book
pprint :: Book -> Text
pprint book = title <> subtitle <> volume <> voltitle
  where title = bookTitle book
        subtitle
          | not . null $ bookSubtitle book = ": " <> bookSubtitle book
          | otherwise = ""
        volume
          | not . null $ bookVolume book =
            if not . null $ bookFascicle book
              then " (vol. " <> bookVolume book <> "; fas. " <> bookFascicle book <> ")"
              else " (vol. " <> bookVolume book <> ")"
          | not . null $ bookFascicle book = " (fas. " <> bookFascicle book <> ")"
          | otherwise = ""
        voltitle
          | not . null $ bookVoltitle book = " " <> bookVoltitle book
          | otherwise = ""

-- |A null book
emptyBook :: Book
emptyBook = Book Nothing "" "" "" "" "" "" "" Nothing Nothing Nothing False Nothing "" "" Uncategorised

-- |Choice
bool :: Bool -- ^ Condition
     -> Text -- ^ True case
     -> Text -- ^ False case
     -> Text
bool True  t _ = t
bool False _ f = f
