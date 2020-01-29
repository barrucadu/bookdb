{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Database (module Database, module Types) where

import           Data.Maybe                   (listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Database.Selda
import           Database.Selda.MakeSelectors

import           Types

-- | The table of books.
books :: Table Book
(books, dbIsbn :*: dbTitle :*: dbSubtitle :*: dbCover :*: dbVolume :*: dbFascicle :*: dbVoltitle :*: dbAuthor :*: dbTranslator :*: dbEditor :*: dbSorting :*: dbRead :*: dbLastRead :*: dbLocation :*: dbBorrower :*: dbCategoryCode)
  = tableWithSelectors "books" [#bookIsbn :- primary, #bookCategoryCode :- foreignKey categories dbCode]

-- | The table of categories
categories :: Table BookCategory
(categories, dbCode :*: dbName) = tableWithSelectors "book_categories" [#categoryCode :- primary]

-- | Create the tables
makedb :: SeldaM db ()
makedb = do
    createTable categories
    insert_ categories defaultCategories
    createTable books
  where
    defaultCategories =
      [ BookCategory { categoryCode = "CS", categoryName = "Computer Science" }
      , BookCategory { categoryCode = "F", categoryName =  "Miscellaneous Fiction" }
      , BookCategory { categoryCode = "HI", categoryName = "History" }
      , BookCategory { categoryCode = "KF", categoryName = "Kitchen, Food, Recipe" }
      , BookCategory { categoryCode = "M", categoryName =  "Manga" }
      , BookCategory { categoryCode = "NF", categoryName = "Miscellaneous Nonfiction" }
      , BookCategory { categoryCode = "P", categoryName =  "Poetry" }
      , BookCategory { categoryCode = "PH", categoryName = "Philosophy" }
      , BookCategory { categoryCode = "PO", categoryName = "Politics" }
      , BookCategory { categoryCode = "PR", categoryName = "Programming" }
      , BookCategory { categoryCode = "RM", categoryName = "Religion, Mythology, & Folklore" }
      , BookCategory { categoryCode = "RPG", categoryName = "RPG Sourcebooks" }
      , uncategorised
      ]

-------------------------------------------------------------------------------
-- * Queries

-- | All categories.
allCategories :: SeldaM db [BookCategory]
allCategories = query (select categories)

-- | All locations.
allLocations :: SeldaM db [Text]
allLocations = query . distinct $ do
  b <- select books
  pure (b ! dbLocation)

-- | All borrowers.
allBorrowers :: SeldaM db [Text]
allBorrowers = query . distinct $ do
  b <- select books
  pure (b ! dbBorrower)

-- | Find a book by ISBN.
findBook :: Text -> SeldaM db (Maybe Book)
findBook isbn = do
  results <- query $ do
    b <- select books
    restrict (b ! dbIsbn .== literal isbn)
    pure b
  pure (listToMaybe results)

-- | Insert a book.
insertBook :: Book -> SeldaM db ()
insertBook b = insert_ books [b]

-- | Replace a book by ISBN.
replaceBook :: Text -> Book -> SeldaM db ()
replaceBook isbn b = transaction $ do
  deleteBook isbn
  insertBook b

-- | Delete a book by ISBN.
deleteBook :: Text -> SeldaM db ()
deleteBook isbn = deleteFrom_ books (\b -> b ! dbIsbn .== literal isbn)

-- | Search the books.
searchBooks
  :: Text -- ^ Title
  -> Text -- ^ Author
  -> Text -- ^ Location
  -> Text -- ^ Borrower
  -> Maybe BookCategory -- ^ Category (exact match)
  -> Maybe Bool -- ^ Match only read/unread books
  -> SeldaM db [Book]
searchBooks title author location borrower category match = query $ do
  b <- select books
  let l s = literal ("%" <> s <> "%")
  restrict (b ! dbTitle    `like` l title)
  restrict (b ! dbAuthor   `like` l author)
  restrict (b ! dbLocation `like` l location)
  restrict (b ! dbBorrower `like` l borrower)
  case category of
    Just cat ->
      restrict (b ! dbCategoryCode .== literal (categoryCode cat))
    Nothing ->
      pure ()
  case match of
    Just True ->
      restrict (b ! dbRead)
    Just False ->
      restrict (not_ (b ! dbRead))
    Nothing ->
      pure ()
  pure b

-- | Limited form of search: use the given restriction.
restrictBooks :: (Row db Book -> Col db Bool) -> SeldaM db [Book]
restrictBooks f = query $ do
  b <- select books
  restrict (f b)
  pure b
