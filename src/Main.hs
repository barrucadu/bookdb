{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (userError)

import Configuration (ConfigParser, defaults, get', loadConfigFile, reloadConfigFile)
import Control.Arrow ((***), first, second)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (pack, replace)
import Data.Text.Encoding (decodeUtf8)
import Database
import Database.Persist.Sql (ConnectionPool, runMigration, runSqlPool, runSqlPersistMPool)
import Database.Persist.Sqlite (withSqlitePool)
import Handler.Edit
import Handler.Information
import Handler.List
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.HTTP.Types.Status (notFound404, methodNotAllowed405, internalServerError500)
import Network.Wai (requestMethod, queryString)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Requests
import Routes
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (catchIOError)
import Web.Routes.Wai (handleWai)

import qualified Network.Wai.Handler.Warp as W

-- |Run the server
main :: IO ()
main = serve route error500
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

-------------------------

-- |Launch the web server.
serve :: PathInfo r
      => (StdMethod -> r -> Handler r) -- ^ Routing function
      -> (String -> Handler r) -- ^ Top-level error handling function
      -> IO ()
serve route on500 = do
  args <- getArgs

  when (length args < 1) $
    die "Expected at least one argument"

  let confFile = case args of
                   (_:conffile:_) -> Just conffile
                   _ -> Nothing

  config <- case confFile of
             Just cfile -> loadConfigFile cfile
             Nothing -> return $ Just defaults

  case config of
    Just conf ->
      let connstr = get' conf "bookdb" "database_file" 
          pool    = withSqlitePool (fromString connstr) 10
      in case head args of
           "run"     -> run route on500 confFile pool conf
           "migrate" -> runNoLoggingT . pool $ liftIO . runSqlPersistMPool (runMigration migrateAll)
           _         -> die "Unknown command, expected 'run' or 'migrate'."

    Nothing -> die "Failed to read configuration"

-- |Die with a fatal error
die :: String -- ^ The error description
    -> IO ()
die err = putStrLn err >> exitFailure

-------------------------

-- |Serve requests
run :: PathInfo r
    => (StdMethod -> r -> Handler r) -- ^ Routing function
    -> (String -> Handler r) -- ^ Top-level error handling function
    -> Maybe FilePath -- ^ The config file
    -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ()) -- ^ Database connection pool runner
    -> ConfigParser -- ^ The configuration
    -> IO ()
run route on500 cfile pool conf = do
  let host = get' conf "bookdb" "host"
  let port = get' conf "bookdb" "port"

  let settings = setHost (fromString host) . setPort port $ W.defaultSettings

  putStrLn $ "Listening on " ++ host ++ ":" ++ show port
  runNoLoggingT . pool $ liftIO . runSettings settings . runner

  where
    runner p = handleWai (fromString webroot) $ \mkurl r ->
      -- This is horrific, come up with a better way of doing it
      let mkurl' r' args = replace "%23" "#" . mkurl r' $ map (\(a,b) -> (a, if b == "" then Nothing else Just b)) args
      in staticPolicy (addBase fileroot) $ process p mkurl' r

      where
        webroot  = get' conf "bookdb" "web_root"
        fileroot = get' conf "bookdb" "file_root"

    process p mkurl path req receiver = requestHandler `catchIOError` runError
      where
        requestHandler = runHandler' $ route method path
        runError err   = runHandler' $ on500 (show err)
        runHandler' h  = runHandler h p mkurl req receiver 
        method         = forceEither . parseMethod . requestMethod $ req

    runHandler h p mkurl req receiver = do
      -- Reload the config
      conf' <- case cfile of
                Just cf -> reloadConfigFile conf cf
                Nothing -> return conf

      -- Build the Cry
      (ps, fs) <- parseRequestBody lbsBackEnd req
      let ps' = map (second $ fromMaybe "") $ queryString req
      let cry = Cry { _req    = req
                    , _params = map (decodeUtf8 *** decodeUtf8) (ps ++ ps')
                    , _files  = map (first decodeUtf8) fs
                    , _conf   = conf'
                    , _mkurl  = mkurl
                    }

      (runResourceT . runNoLoggingT . flip runReaderT cry $ runSqlPool h p) >>= receiver
