{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Prelude          hiding (null)

import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.Set         (Set)
import           Data.Text        (Text, null, splitOn, toLower)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Text.Hamlet      (Html, shamletFile)

import           Database

import qualified Data.Set         as S

-------------------------

index :: [BookCategory] -- ^ List of all categories
      -> [Book] -- ^ The list of all books
      -> Text -- ^ Web root
      -> Html
index categories books web_root =
  let title = "BookDB" :: Text
      body  = list categories books (numAuthors books) (Just $ numRead books) False
  in $(shamletFile "templates/wrapper.hamlet")

search :: [BookCategory] -- ^ List of all categories
       -> Text -- ^ The ISBN
       -> Text -- ^ The title
       -> Text -- ^ The subtitle
       -> Text -- ^ The author
       -> Text -- ^ The matcher
       -> Text -- ^ The location
       -> Text -- ^ The borrower
       -> Maybe BookCategory -- ^ The category
       -> [Book] -- ^ Books matching the search
       -> Text -- ^ Web root
       -> Html
search categories isbn btitle subtitle author match location borrower category books web_root =
  let authors = numAuthors books
      read    = numRead books
      title   = "BookDB :: Search" :: Text
      body    = $(shamletFile "templates/search.hamlet")
  in $(shamletFile "templates/wrapper.hamlet")

list :: [BookCategory] -- ^ List of all categories
     -> [Book]    -- ^ The books
     -> Int       -- ^ The number of authors
     -> Maybe Int -- ^ The number of read books
     -> Bool      -- ^ Whether to display in \"thin\" mode
     -> Html
list categories books authors read thin = $(shamletFile "templates/list.hamlet")

-------------------------

addForm :: [BookCategory]  -- ^ List of all categories
        -> Text -- ^ Web root
        -> Html
addForm categories web_root =
  let book   = Nothing
      target = "add" :: Text
      title  = "BookDB :: Add" :: Text
      body   = $(shamletFile "templates/edit_form.hamlet")
  in $(shamletFile "templates/wrapper.hamlet")

editForm :: [BookCategory] -- ^ List of all categories
         -> Book -- ^ The book to edit
         -> Text -- ^ Web root
         -> Html
editForm categories bk web_root =
  let book   = Just bk
      target = "edit/" <> bookIsbn bk
      title  = "BookDB :: Edit " <> bookIsbn bk :: Text
      body   = $(shamletFile "templates/edit_form.hamlet")
  in $(shamletFile "templates/wrapper.hamlet")

confirmDelete :: Book -- ^ The book to delete
              -> Text -- ^ Web root
              -> Html
confirmDelete book web_root = $(shamletFile "templates/confirm_delete.hamlet")

-------------------------

notice :: Text -- ^ The message
       -> Text -- ^ Web root
       -> Html
notice message web_root =
  let error = False
  in $(shamletFile "templates/information.hamlet")

noticeError :: Text -- ^ The message
            -> Text -- ^ Web root
            -> Html
noticeError message web_root =
  let error = True
  in $(shamletFile "templates/information.hamlet")

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

-- |Choice
bool :: Bool -- ^ Condition
     -> Text -- ^ True case
     -> Text -- ^ False case
     -> Text
bool True  t _ = t
bool False _ f = f
