module Handler.List
    ( index
    , search
    , restrict
    ) where

import Routes

import Data.Text (Text)
import Database.Persist
import Web.Seacat

-------------------------

index :: Handler Sitemap
index = undefined

search :: Handler Sitemap
search = undefined

restrict :: EntityField v t -- ^ The field to filter on
         -> t -- ^ the value to filter by
         -> Handler Sitemap
restrict field is = undefined
