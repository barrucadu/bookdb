{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( -- * Display forms
      add
    , edit
    , delete

    -- * Save changes
    , commitAdd
    , commitEdit
    , commitDelete
    ) where

import Prelude hiding (userError)

import Data.Text (Text)
import Database
import Database.Persist (Entity(..), (==.), selectFirst)
import Handler.Information (userError)
import Routes
import Web.Seacat

import qualified Handler.Templates as T

-- |Display an add form, or an error if in read-only mode
add :: Handler Sitemap
add = onReadWrite add'

-- |Display an edit form, or an error if in read-only mode
edit :: Text -- ^ The ISBN
     -> Handler Sitemap
edit = onReadWrite . withBook edit'

-- |Display a confirm delete page, or an error if in read-only mode
delete :: Text -- ^ The ISBN
       -> Handler Sitemap
delete = onReadWrite . withBook delete'

-- |Commit an add, or display an error if in read-only mode
commitAdd :: Handler Sitemap
commitAdd = onReadWrite commitAdd'

-- |Commit an edit, or display an error if in read-only mode
commitEdit :: Text -- ^ The ISBN
           -> Handler Sitemap
commitEdit = onReadWrite . withBook commitEdit'

-- |Commit a delete, or display an error if in read-only mode
commitDelete :: Text -- ^ The ISBN
             -> Handler Sitemap
commitDelete = onReadWrite . withBook commitDelete'

-------------------------

-- |Run the given handler if in read-write mode, otherwise display an
-- error page.
onReadWrite :: Handler Sitemap -- ^ The handler
            -> Handler Sitemap
onReadWrite handler = do
  readonly <- conf' "server" "readonly"

  if readonly
  then userError "Database is read-only"
  else handler

-- |Run a handler which tskes a book as an argument, identified by
-- ISBN, and display an error if there is no such book.
withBook :: (Entity Book -> Handler Sitemap) -- ^ The handler
         -> Text -- ^ The ISBN
         -> Handler Sitemap
withBook handler isbn = do
  book <- selectFirst [BookIsbn ==. isbn] []
  case book of
    Just b  -> handler b
    Nothing -> userError "No such book"

-------------------------

add' :: Handler Sitemap
add' = undefined

edit' :: Entity Book -> Handler Sitemap
edit' (Entity bookId book) = undefined

delete' :: Entity Book -> Handler Sitemap
delete' (Entity bookId book) = undefined

-------------------------

commitAdd' :: Handler Sitemap
commitAdd' = undefined

commitEdit' :: Entity Book -> Handler Sitemap
commitEdit' (Entity bookId book) = undefined

commitDelete' :: Entity Book -> Handler Sitemap
commitDelete' (Entity bookId book) = undefined