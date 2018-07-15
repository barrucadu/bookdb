{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- | Additional types and type utilities.
module Types where

import GHC.Generics (Generic)
import Data.Maybe (listToMaybe)
import Data.Text (Text, toLower)
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
  , bookLocation :: Text
  , bookBorrower :: Text
  , bookCategory :: BookCategory
  }
  deriving (Generic, Show)

-- |A null book
emptyBook :: Book
emptyBook = Book "" "" "" Nothing "" "" "" "" Nothing Nothing Nothing False Nothing False "" "" Uncategorised

--------------------------------------------------------------------------------
-- Book Categories

data BookCategory = ComputerScience | Programming | Philosophy | Politics | History | ReligionAndMythology | Poetry | Manga | MiscFiction | MiscNonFiction | Uncategorised
  deriving (Eq, Enum, Bounded, Generic, Read, Show)

-- | All categories, in the order they appear in lists in the
-- front-end.
allCategories :: [BookCategory]
allCategories = [ComputerScience .. Uncategorised]

-- | Get the short code name for each category
categoryCode :: BookCategory -> Text
categoryCode ComputerScience = "CS"
categoryCode Programming     = "PR"
categoryCode Philosophy      = "PH"
categoryCode Politics        = "PO"
categoryCode History         = "HI"
categoryCode ReligionAndMythology = "RM"
categoryCode Poetry          = "P"
categoryCode Manga           = "M"
categoryCode MiscFiction     = "F"
categoryCode MiscNonFiction  = "NF"
categoryCode Uncategorised   = "-"

-- | Get the long name for each category
categoryName :: BookCategory -> Text
categoryName ComputerScience = "Computer Science"
categoryName Programming     = "Programming"
categoryName Philosophy      = "Philosophy"
categoryName Politics        = "Politics"
categoryName History         = "History"
categoryName ReligionAndMythology = "Religion, Mythology, & Folklore"
categoryName Poetry          = "Poetry"
categoryName Manga           = "Manga"
categoryName MiscFiction     = "Miscellaneous Fiction"
categoryName MiscNonFiction  = "Miscellaneous Nonfiction"
categoryName Uncategorised   = "Uncategorised"

-- | Category code reverse lookup (case insensitive)
categoryOf :: Text -> Maybe BookCategory
categoryOf code = listToMaybe [cat | cat <- allCategories, toLower (categoryCode cat) == toLower code]
