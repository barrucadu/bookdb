{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Configuration
import           Control.Monad.Trans.Reader    (runReaderT)
import           Data.Foldable                 (for_)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Database
import           Database.Selda                (like, literal, not_, (!), (.==))
import qualified Database.Selda.PostgreSQL     as DB
import           Handler.Edit
import           Handler.Information
import           Handler.List
import           Network.HTTP.Types.Status     (internalServerError500,
                                                notFound404)
import           Network.Wai                   (pathInfo)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)
import qualified Web.Scotty.Trans              as S

-- |Run the server
main :: IO ()
main = getConfig >>= \case
  Right conf -> getArgs >>= \case
    ["run"] -> serve conf
    ["makedb"] -> DB.withPostgreSQL (cfgDatabase conf) makedb
    _ -> do
      putStrLn "USAGE: bookdb (run | makedb)"
      exitFailure
  Left errors -> do
    putStrLn "Couldn't load configuration from environment:"
    for_ errors $ \e -> putStrLn ("    " ++ e)
    exitFailure

-- | Serve requests.
--
-- Each connection gets its own DB connection.
serve :: Configuration -> IO ()
serve conf = S.scottyT port run $ do
    S.middleware $ staticPolicy (addBase (cfgFileRoot conf))

    S.get "/"     $ S.redirect "/search"
    S.get "/list" $ S.redirect "/search"

    S.get "/search" search

    S.get "/read"   $ restrict (! dbRead)
    S.get "/unread" $ restrict (\b -> not_ (b ! dbRead))

    S.get "/author/:author" $ do
      author <- S.param "author"
      restrict (\b -> b ! dbAuthor `like` literal ("%" <> author <> "%"))

    S.get "/translator/:translator" $ do
      translator <- S.param "translator"
      restrict (\b -> b ! dbTranslator .== literal (Just translator))

    S.get "/editor/:editor" $ do
      editor <- S.param "editor"
      restrict (\b -> b ! dbEditor .== literal (Just editor))

    S.get "/location/:location" $ do
      location <- S.param "location"
      restrict (\b -> b ! dbLocation .== literal location)

    S.get "/category/:category" $ do
      category <- S.param "category"
      restrict (\b -> b ! dbCategoryCode .== literal category)

    S.get "/borrower/:borrower" $ do
      borrower <- S.param "borrower"
      restrict (\b -> b ! dbBorrower .== literal borrower)

    S.get "/add"  add
    S.post "/add" commitAdd

    S.get "/edit/:key"  $ do
      key <- S.param "key"
      edit key
    S.post "/edit/:key" $ do
      key <- S.param "key"
      commitEdit key

    S.get "/delete/:key"  $ do
      key <- S.param "key"
      delete key
    S.post "/delete/:key" $ do
      key <- S.param "key"
      commitDelete key

    S.notFound $ do
      path <- pathInfo <$> S.request
      error404 (T.intercalate "/" path)

    S.defaultHandler (error500 . TL.toStrict)
  where
    port = cfgPort conf
    run ma = DB.withPostgreSQL (cfgDatabase conf) (runReaderT ma conf)

-------------------------

-- |404 handler.
error404 :: T.Text -> Handler db
error404 uri = serverError notFound404 ("File not found: " <> uri)

-- |500 handler.
error500 :: T.Text -> Handler db
error500 = serverError internalServerError500
