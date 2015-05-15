{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (userError)

import Data.ConfigFile (emptyCP, readstring)
import Data.Either.Utils (forceEither)
import Data.Text (pack)
import Database
import Handler.Edit
import Handler.Information
import Handler.List
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types.Status (notFound404, methodNotAllowed405, internalServerError500)
import Routes
import Server

-- |Run the server
main :: IO ()
main = seacat route error500 $ defaultSettings { _config  = Just defaults
                                               , _migrate = Just migrateAll
                                               }
  where error500 = serverError internalServerError500 . pack

-- |Route a request to a handler. These do not cover static files, as
-- Seacat handles those.
route :: StdMethod -> Sitemap -> Handler Sitemap
route GET Booklist = index
route GET Search   = search

-- Fuzzy matching is required for author because in general the author
-- field contains a list, and we want to be able to match any one of
-- them.
route GET (Author a)     = restrictFuzzy BookAuthor a
route GET (Translator t) = restrict BookTranslator (Just t)
route GET (Editor e)     = restrict BookEditor (Just e)
route GET Read           = restrict BookRead True
route GET Unread         = restrict BookRead False
route GET (Location l)   = restrict BookLocation l
route GET (Borrower b)   = restrict BookBorrower b

route GET Add        = add
route GET (Edit e)   = edit e
route GET (Delete d) = delete d

route POST Add        = commitAdd
route POST (Edit e)   = commitEdit e
route POST (Delete d) = commitDelete d

route _ Error404 = serverError notFound404 "File not found"

route _ _ = serverError methodNotAllowed405 "Method not allowed"

-- |Default configuration
defaults :: ConfigParser
defaults = forceEither . readstring emptyCP $ unlines
  [ "[server]"
  , "readonly = false"
  , "[database]"
  , "connection_string = bookdb.sqlite"
  ]
