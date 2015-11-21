{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Additional types and type utilities.
module Types where

import Data.Maybe (listToMaybe)
import Data.Text (Text, toLower)
import Database.Persist.TH

--------------------------------------------------------------------------------
-- Book Categories

data BookCategory = ComputerScience | Programming | Philosophy | Politics | History | ReligionAndMythology | Folklore | Poetry | Manga | MiscFiction | MiscNonFiction | Uncategorised
  deriving (Eq, Enum, Bounded, Read, Show)

derivePersistField "BookCategory"

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
categoryCode Folklore        = "FO"
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
categoryName ReligionAndMythology = "Religion & Mythology"
categoryName Folklore        = "Folklore"
categoryName Poetry          = "Poetry"
categoryName Manga           = "Manga"
categoryName MiscFiction     = "Miscellaneous Fiction"
categoryName MiscNonFiction  = "Miscellaneous Nonfiction"
categoryName Uncategorised   = "Uncategorised"

-- | Category code reverse lookup (case insensitive)
categoryOf :: Text -> Maybe BookCategory
categoryOf code = listToMaybe [cat | cat <- allCategories, toLower (categoryCode cat) == toLower code]
