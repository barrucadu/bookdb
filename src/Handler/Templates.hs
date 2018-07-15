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
    , stats
    , search
    , index
    ) where

import           Prelude          hiding (null)

import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.Set         (Set)
import           Data.Text        (Text, null, pack, splitOn, toLower)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Numeric          (showFFloat)
import           Text.Hamlet      (HtmlUrl, hamletFile)
import qualified Text.Show        (showListWith)

import           Database
import           Routes

import qualified Data.Set         as S

-------------------------

index :: [BookCategory] -- ^ List of all categories
      -> [Book] -- ^ The list of all books
      -> HtmlUrl Sitemap
index categories books =
  let title = "BookDB" :: Text
      body  = list categories books (numAuthors books) (Just $ numRead books) False
  in $(hamletFile "templates/wrapper.hamlet")

search :: [BookCategory] -- ^ List of all categories
       -> Text -- ^ The ISBN
       -> Text -- ^ The title
       -> Text -- ^ The subtitle
       -> Text -- ^ The author
       -> Bool -- ^ Whether to match read
       -> Bool -- ^ Whether to match unread
       -> Text -- ^ The location
       -> Text -- ^ The borrower
       -> BookCategory -- ^ The category
       -> [Book] -- ^ Books matching the search
       -> HtmlUrl Sitemap
search categories isbn btitle subtitle author matchread matchunread location borrower category books =
  let authors = numAuthors books
      read    = numRead books
      title   = "BookDB :: Search" :: Text
      body    = $(hamletFile "templates/search.hamlet")
  in $(hamletFile "templates/wrapper.hamlet")

list :: [BookCategory] -- ^ List of all categories
     -> [Book]    -- ^ The books
     -> Int       -- ^ The number of authors
     -> Maybe Int -- ^ The number of read books
     -> Bool      -- ^ Whether to display in \"thin\" mode
     -> HtmlUrl Sitemap
list categories books authors read thin = $(hamletFile "templates/list.hamlet")

stats :: [BookCategory] -- ^ List of all categories
      -> [Book]  -- ^ The books read in the prior year
      -> [Book]  -- ^ The least recently read books
      -> [Int]   -- ^ The number of books read each month this year
      -> [Int]   -- ^ The number of books read each month last year
      -> [Float] -- ^ The number of books read each month on average
      -> HtmlUrl Sitemap
stats categories lastYearBooks leastRecentBooks thisYear lastYear avgYear =
  let title = "BookDB :: Stats" :: Text
      lastYearAuthors    = numAuthors lastYearBooks
      leastRecentAuthors = numAuthors leastRecentBooks
      body = $(hamletFile "templates/stats.hamlet")
  in $(hamletFile "templates/wrapper.hamlet")

-------------------------

addForm :: [BookCategory]  -- ^ List of all categories
        -> HtmlUrl Sitemap
addForm categories =
  let book   = Nothing
      target = Add
      title  = "BookDB :: Add" :: Text
      body   = $(hamletFile "templates/edit_form.hamlet")
  in $(hamletFile "templates/wrapper.hamlet")

editForm :: [BookCategory] -- ^ List of all categories
         -> Book -- ^ The book to edit
         -> HtmlUrl Sitemap
editForm categories bk =
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

-- |Choice
bool :: Bool -- ^ Condition
     -> Text -- ^ True case
     -> Text -- ^ False case
     -> Text
bool True  t _ = t
bool False _ f = f

-- |Show a float to two decimal places at most.
showFloat :: Float -> String
showFloat n = showFFloat (Just 2) n ""

-- |Show a list, with a function to show the elements.
showListWith :: (a -> String) -> [a] -> String
showListWith f xs = Text.Show.showListWith ((++) . f) xs ""
