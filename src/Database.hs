{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures -fsimplifier-phases=0 #-}

module Database (module Database, module Types) where

import           Control.Monad          (unless)
import           Data.Maybe             (listToMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Database.Selda
import           Database.Selda.Generic

import           Types

-- | The table of books.
books :: GenTable Book
books = genTable "books" [bookIsbn :- primaryGen, bookCategoryCode :- fkGen (gen categories) dbCode]

-- | The table of categories
categories :: GenTable BookCategory
categories = genTable "book_categories" [categoryCode :- primaryGen]

-- | Create the tables
migrate :: SeldaM ()
migrate = do
  createTable (gen books)
  createTable (gen categories)

-- selectors
dbIsbn :*: dbTitle :*: dbSubtitle :*: dbCover :*: dbVolume :*: dbFascicle :*: dbVoltitle :*: dbAuthor :*: dbTranslator :*: dbEditor :*: dbSorting :*: dbRead :*: dbLastRead :*: dbNowReading :*: dbLocation :*: dbBorrower :*: dbCategoryCode = selectors (gen books)

dbCode :*: dbName = selectors (gen categories)

-------------------------------------------------------------------------------
-- * Queries

-- | All books.
allBooks :: SeldaM [Book]
allBooks = map fromRel <$> query (select (gen books))

-- | All categories.
allCategories :: SeldaM [BookCategory]
allCategories = map fromRel <$> query (select (gen categories))

-- | Find a book by ISBN.
findBook :: Text -> SeldaM (Maybe Book)
findBook isbn = do
  results <- query $ do
    b <- select (gen books)
    restrict (b ! dbIsbn .== literal isbn)
    pure b
  pure (listToMaybe (map fromRel results))

-- | Insert a book.
insertBook :: Book -> SeldaM ()
insertBook b = insertGen_ books [b]

-- | Replace a book by ISBN.
replaceBook :: Text -> Book -> SeldaM ()
replaceBook isbn b = transaction $ do
  deleteBook isbn
  insertBook b

-- | Delete a book by ISBN.
deleteBook :: Text -> SeldaM ()
deleteBook isbn = deleteFrom_ (gen books) (\b -> b ! dbIsbn .== literal isbn)

-- | List books read since a given time.
readSince :: UTCTime -> SeldaM [Book]
readSince lastRead = do
  results <- query $ do
    b <- select (gen books)
    restrict (b ! dbRead)
    restrict (b ! dbLastRead .>= literal (Just lastRead))
    order (b ! dbLastRead) descending
    pure b
  pure (map fromRel results)

-- | List read books, least recently read first
leastRecent :: SeldaM [Book]
leastRecent = do
  results <- query $ do
    b <- select (gen books)
    restrict (b ! dbRead)
    order (b ! dbLastRead) ascending
    pure b
  pure (map fromRel results)

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
searchBooks isbn title subtitle author location borrower category matchread matchunread = do
  results <- query $ do
    b <- select (gen books)
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
  pure (map fromRel results)

-- | Limited form of search: use the given restriction.
restrictBooks :: (Cols s (Relation Book) -> Col s Bool) -> SeldaM [Book]
restrictBooks f = do
  results <- query $ do
    b <- select (gen books)
    restrict (f b)
    pure b
  pure (map fromRel results)
