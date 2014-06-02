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

import Control.Applicative ((<$>))
import Data.Text (Text)
import Database
import Database.Persist (Entity(..))
import Handler.Utils
import Routes
import Web.Seacat
import Web.Seacat.RequestHandler (htmlUrlResponse)

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

add' :: Handler Sitemap
add' = do
  suggestion <- suggest
  htmlUrlResponse $ T.addForm suggestion

edit' :: Entity Book -> Handler Sitemap
edit' (Entity bookId book) = do
  suggestion <- suggest
  htmlUrlResponse $ T.editForm suggestion book

delete' :: Entity Book -> Handler Sitemap
delete' (Entity bookId book) = htmlUrlResponse $ T.confirmDelete book

-------------------------

commitAdd' :: Handler Sitemap
commitAdd' = undefined

commitEdit' :: Entity Book -> Handler Sitemap
commitEdit' (Entity bookId book) = undefined

commitDelete' :: Entity Book -> Handler Sitemap
commitDelete' (Entity bookId book) = undefined