{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- | Additional types and type utilities.
module Types where

import GHC.Generics (Generic)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)

-------------------------------------------------------------------------------
-- Books

data Book = Book
  { bookIsbn     :: Text
  , bookTitle    :: Text
  , bookSubtitle :: Text

  -- The filename of the cover image
  , bookCover :: Maybe Text

  -- It might appear odd not to have the volume and fascicle numbers
  -- as ints, but then that would prevent things like "4a" being a
  -- valid volume number. For something as flexible as books, using
  -- Text is probably the most future-proof.
  , bookVolume   :: Text
  , bookFascicle :: Text
  , bookVoltitle :: Text

  -- Multiple authors can be separated by " & ", and they will be
  -- displayed in a list on the book list page. Perhaps a [Text] would
  -- be more appropriate here, but then it would need to be serialised
  -- when editing and unserialised when adding. Just using a Text
  -- means it only has to be unserialised in one place.
  , bookAuthor     :: Text
  , bookTranslator :: Maybe Text
  , bookEditor     :: Maybe Text

  -- Optional sorting key field. If this is Nothing, the author is
  -- used instead.
  , bookSorting :: Maybe Text

  -- lastread is only a Maybe because I don't have last read dates for
  -- all my books: only those since I first made bookdb. Really, I
  -- should re-read those books which I haven't touched for that long.
  , bookRead       :: Bool
  , bookLastRead   :: Maybe UTCTime
  , bookNowReading :: Bool

  -- These work, if I only have one copy of each book. If I have
  -- multiple copies, things start to get a bit messy.
  , bookLocation     :: Text
  , bookBorrower     :: Text
  , bookCategoryCode :: Text
  }
  deriving (Generic, Show)

-- |A null book
emptyBook :: Book
emptyBook = Book "" "" "" Nothing "" "" "" "" Nothing Nothing Nothing False Nothing False "" "" "-"

-- | A book's category.  Returns the "Uncategorised" category if
-- there's no match.
bookCategory :: Book -> [BookCategory] -> BookCategory
bookCategory = categoryByCode . bookCategoryCode

--------------------------------------------------------------------------------
-- Book Categories

data BookCategory = BookCategory
  { categoryCode :: Text
  , categoryName :: Text
  }
  deriving (Generic, Eq, Show)

-- | Find a category by code.  Returns 'uncategorised' if there's no
-- match.
categoryByCode :: Text -> [BookCategory] -> BookCategory
categoryByCode code = fromMaybe uncategorised . categoryByCode' code

-- | Find a category by code.
categoryByCode' :: Text -> [BookCategory] -> Maybe BookCategory
categoryByCode' code = find (\c -> categoryCode c == code)

-- | The "uncategorised" category.
uncategorised :: BookCategory
uncategorised = BookCategory
  { categoryCode = "-"
  , categoryName = "Uncategorised"
  }
