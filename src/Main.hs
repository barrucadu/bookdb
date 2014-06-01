module Main where

import Database
import Handler.List
import Handler.Edit
import Routes

import Data.ConfigFile (emptyCP, readstring)
import Data.Either.Utils (forceEither)
import Data.Text (Text, pack)
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types.Status (notFound404, methodNotAllowed405, internalServerError500)
import Web.Seacat

-------------------------

-- |Run the server
main :: IO ()
main = seacat route error500 $ defaultSettings { _config  = Just defaults
                                               , _migrate = Just migrateAll
                                               }

-- |Route a request to a handler. These do not cover static files, as
-- Seacat handles those.
route :: StdMethod -> Sitemap -> Handler Sitemap
route GET Booklist = index
route GET Search   = search

route GET (Author a)     = restrict BookAuthor a
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

route _ Error404 = error404 "File not found"

route _ _ = error405 "Method not allowed"

-- |Default configuration
defaults :: ConfigParser
defaults = forceEither . readstring emptyCP $ unlines
  [ "[server]"
  , "readonly = false"
  , "[database]"
  , "connection_string = bookdb.sqlite"
  ]

-- |Display a 404 error
-- Todo: prettier error pages
error404 :: String -> Handler Sitemap
error404 = textResponse' notFound404 . pack

-- |Display a 405 error
error405 :: String -> Handler Sitemap
error405 = textResponse' methodNotAllowed405 . pack

-- |Display a 500 error
error500 :: String -> Handler Sitemap
error500 = textResponse' internalServerError500 . pack
