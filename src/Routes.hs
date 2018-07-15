{-# LANGUAGE TemplateHaskell #-}

module Routes where

import Data.Text (Text, toLower)
import Web.Routes (PathInfo(..), pToken)
import Web.Routes.TH (derivePathInfo', standard)

import Types

-- |The possible routes in bookdb
data Sitemap =
    List
  -- ^ The full book list
  | Search
  -- ^ The search form and results
  | Stats
  -- ^ Reading statistics

  | Author Text
  -- ^ Filter by author
  | Translator Text
  -- ^ Filter by translator
  | Editor Text
  -- ^ Filter by editor
  | Read
  -- ^ Filter by read
  | Unread
  -- ^ Filter by unread
  | Location Text
  -- ^ Filter by location
  | Category BookCategory
  -- ^ Filter by category
  | Borrower Text
  -- ^ Filter by borrower

  | Covers Text
  -- ^ A book cover image
  | Static Text
  -- ^ A static file

  | Add
  -- ^ Add a new book
  | Edit Text
  -- ^ Edit a book by ISBN
  | Delete Text
  -- ^ Delete a book by ISBN

  deriving (Read, Show)

-- Orphan, but I think keeping ALL the URL encoding/decoding code in
-- this one file is best.
instance PathInfo BookCategory where
  toPathSegments c = [toLower $ categoryCode c]
  fromPathSegments = pToken () categoryOf

$(derivePathInfo' standard ''Sitemap)
