{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Database where

import Database.Persist.TH
import Data.Time (UTCTime)
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book
    -- The filename of the cover image
    cover Text Maybe

    isbn     Text
    title    Text
    subtitle Text

    -- It might appear odd not to have the volume and fascicle numbers
    -- as ints, but then that would prevent things like "4a" being a
    -- valid volume number. For something as flexible as books, using
    -- Text is probably the most future-proof.
    volume   Text
    fascicle Text
    voltitle Text

    -- Multiple authors can be sepaarted by "and", and they will be
    -- displayed in a list on the book list page. Perhaps a [Text]
    -- would be more appropriate here, but then it would need to be
    -- serialised when editing and unserialised when adding. Just
    -- using a Text means it only has to be unserialised in one place.
    author     Text
    translator Text Maybe
    editor     Text Maybe

    -- lastread is only a Maybe because I don't have last read dates
    -- for all my books: only those since I first made bookdb. Really,
    -- I should re-read those books which I haven't touched for that
    -- long.
    read     Bool
    lastread UTCTime Maybe

    -- These work, if I only have one copy of each book. If I have
    -- multiple copies, things start to get a bit messy.
    location Text
    borrower Text

    -- Not really sure if I want to keep these, I never use those
    -- fields. I'll keep them for now, as I port the Python version to
    -- Haskell, but may well drop them later.
    quote Text
    notes Text

    UniqueBookIsbn isbn

    deriving Read
    deriving Show
|]
