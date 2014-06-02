{-# LANGUAGE OverloadedStrings #-}

module Handler.List
    ( index
    , search
    , restrict
    ) where

import Database
import Handler.Utils
import Routes

import qualified Handler.Templates as T

import Control.Arrow (second)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Database.Esqueleto hiding ((==.))
import Database.Persist hiding ((||.))
import Web.Seacat
import Web.Seacat.RequestHandler (htmlUrlResponse)

-------------------------

index :: Handler Sitemap
index = do
  suggestion <- suggest
  books <- selectList [] [ Asc BookAuthor
                        , Asc BookTitle
                        , Asc BookVolume
                        , Asc BookFascicle]

  let books' = map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.index suggestion books'

search :: Handler Sitemap
search = do
  suggestion <- suggest

  isbn        <- param "isbn"
  title       <- param "title"
  subtitle    <- param "subtitle"
  author      <- param "author"
  matchread   <- hasParam "matchread"
  matchunread <- hasParam "matchunread"
  location    <- param "location"
  borrower    <- param "borrower"

  books <- select $
          from $ \b -> do
            when (isJust isbn) $
              where_ (b ^. BookIsbn     `like` (%) ++. val (fromJust isbn) ++. (%))
            when (isJust title) $
              where_ (b ^. BookTitle    `like` (%) ++. val (fromJust title) ++. (%))
            when (isJust subtitle) $
              where_ (b ^. BookSubtitle `like` (%) ++. val (fromJust title) ++. (%))
            when (isJust author) $
              where_ (b ^. BookAuthor   `like` (%) ++. val (fromJust author) ++. (%))
            when (isJust location) $
              where_ (b ^. BookLocation `like` (%) ++. val (fromJust location) ++. (%))
            when (isJust borrower) $
              where_ (b ^. BookBorrower `like` (%) ++. val (fromJust borrower) ++. (%))
            where_ ((b ^. BookRead &&. val matchread) ||. (not_ (b ^. BookRead) &&. val matchunread))
            orderBy [ asc (b ^. BookAuthor)
                    , asc (b ^. BookTitle)
                    , asc (b ^. BookVolume)
                    , asc (b ^. BookFascicle)
                    ]
            return b

  let books' = map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.search suggestion isbn title subtitle author matchread matchunread location borrower books'


restrict :: PersistField t
         => EntityField (BookGeneric SqlBackend) t -- ^ The field to filter on
         -> t -- ^ the value to filter by
         -> Handler Sitemap
restrict field is = do
  suggestion <- suggest
  books <- selectList [ field ==. is ]
                     [ Asc BookAuthor
                     , Asc BookTitle
                     , Asc BookVolume
                     , Asc BookFascicle
                     ]

  let books' = map (\(Entity _ e) -> e) books

  htmlUrlResponse $ T.index suggestion books'
