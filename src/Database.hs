{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Database (module Database, module Types) where

import           Control.Monad          (unless)
import           Data.Maybe             (listToMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Database.Selda

import           Types

-- | The table of books.
books :: Table Book
books = table "books" [#bookIsbn :- primary, #bookCategoryCode :- foreignKey categories dbCode]

-- | The table of categories
categories :: Table BookCategory
categories = table "book_categories" [#categoryCode :- primary]

-- | Create the tables
migrate :: SeldaM ()
migrate = do
  createTable categories
  createTable books

-- selectors
dbIsbn :*: dbTitle :*: dbSubtitle :*: dbCover :*: dbVolume :*: dbFascicle :*: dbVoltitle :*: dbAuthor :*: dbTranslator :*: dbEditor :*: dbSorting :*: dbRead :*: dbLastRead :*: dbLocation :*: dbBorrower :*: dbCategoryCode = selectors books

dbCode :*: dbName = selectors categories

-------------------------------------------------------------------------------
-- * Queries

-- | All books.
allBooks :: SeldaM [Book]
allBooks = query (select books)

-- | All categories.
allCategories :: SeldaM [BookCategory]
allCategories = query (select categories)

-- | Find a book by ISBN.
findBook :: Text -> SeldaM (Maybe Book)
findBook isbn = do
  results <- query $ do
    b <- select books
    restrict (b ! dbIsbn .== literal isbn)
    pure b
  pure (listToMaybe results)

-- | Insert a book.
insertBook :: Book -> SeldaM ()
insertBook b = insert_ books [b]

-- | Replace a book by ISBN.
replaceBook :: Text -> Book -> SeldaM ()
replaceBook isbn b = transaction $ do
  deleteBook isbn
  insertBook b

-- | Delete a book by ISBN.
deleteBook :: Text -> SeldaM ()
deleteBook isbn = deleteFrom_ books (\b -> b ! dbIsbn .== literal isbn)

-- | Search the books.
searchBooks
  :: Text -- ^ ISBN
  -> Text -- ^ Title
  -> Text -- ^ Subtitle
  -> Text -- ^ Author
  -> Text -- ^ Location
  -> Text -- ^ Borrower
  -> Maybe BookCategory -- ^ Category (exact match)
  -> Bool -- ^ Permit read books
  -> Bool -- ^ Permit unread books
  -> SeldaM [Book]
searchBooks isbn title subtitle author location borrower category matchread matchunread = query $ do
  b <- select books
  let l s = literal ("%" <> s <> "%")
  restrict (b ! dbIsbn     `like` l isbn)
  restrict (b ! dbTitle    `like` l title)
  restrict (b ! dbSubtitle `like` l subtitle)
  restrict (b ! dbAuthor   `like` l author)
  restrict (b ! dbLocation `like` l location)
  restrict (b ! dbBorrower `like` l borrower)
  case category of
    Just cat ->
      restrict (b ! dbCategoryCode .== literal (categoryCode cat))
    Nothing ->
      pure ()
  unless matchread $
    restrict (not_ (b ! dbRead))
  unless matchunread $
    restrict (b ! dbRead)
  pure b

-- | Limited form of search: use the given restriction.
restrictBooks :: (Row s Book -> Col s Bool) -> SeldaM [Book]
restrictBooks f = query $ do
  b <- select books
  restrict (f b)
  pure b
