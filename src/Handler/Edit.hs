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

import Data.Text (Text)
import Routes
import Web.Seacat

import qualified Handler.Templates as T

add :: Handler Sitemap
add = undefined

edit :: Text -- ^ The ISBN
     -> Handler Sitemap
edit isbn = undefined

delete :: Text -- ^ The ISBN
       -> Handler Sitemap
delete isbn = undefined

-------------------------

commitAdd :: Handler Sitemap
commitAdd = undefined

commitEdit :: Text -- ^ The ISBN
           -> Handler Sitemap
commitEdit isbn = undefined

commitDelete :: Text -- ^ The ISBN
             -> Handler Sitemap
commitDelete isbn = undefined