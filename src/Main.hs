{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (userError)

import Configuration (ConfigParser, defaults, get, loadConfigFile)
import Control.Arrow ((***), first, second)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, LogLevel(..), logInfoN, logErrorN, filterLogger, runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Database
import Database.Persist.Sql (ConnectionPool, runMigration, runSqlPool)
import Database.Persist.Sqlite (withSqlitePool)
import Handler.Edit
import Handler.Information
import Handler.List
import Handler.Stats
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.HTTP.Types.Status (notFound404, methodNotAllowed405, internalServerError500)
import Network.Wai (remoteHost, rawPathInfo, requestMethod, queryString)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Requests
import Routes
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (catchIOError)
import Web.Routes (fromPathInfo, toPathInfoParams)
import Web.Routes.Wai (handleWai_)

import qualified Data.ByteString.Char8 as B8
import qualified Network.Wai.Handler.Warp as W

-- |Run the server
main :: IO ()
main = runLogging defaults $ do
  args <- liftIO getArgs

  when (length args < 1) $
    logErrorN "Expected at least one argument" >> liftIO exitFailure

  config <- case args of
             (_:conffile:_) -> liftIO $ loadConfigFile conffile
             _ -> return $ Just defaults

  case config of
    Just conf ->
      let connstr = get conf "database_file"
       in liftIO . runLogging conf . withSqlitePool (fromString connstr) 10 $ case head args of
           "run"     -> serve route error404 error500 conf
           "migrate" -> runSqlPool (runMigration migrateAll)
           _         -> const $ logErrorN "Unknown command, expected 'run' or 'migrate'." >> liftIO exitFailure

    Nothing -> logErrorN "Failed to read configuration" >> liftIO exitFailure

-- |Route a request to a handler. These do not cover static files, as
-- Seacat handles those.
route :: StdMethod -> Sitemap -> Handler Sitemap
route GET List   = list
route GET Search = search
route GET Stats  = stats

-- Fuzzy matching is required for author because in general the author
-- field contains a list, and we want to be able to match any one of
-- them.
route GET (Author a)     = restrictFuzzy BookAuthor a
route GET (Translator t) = restrict BookTranslator (Just t)
route GET (Editor e)     = restrict BookEditor (Just e)
route GET Read           = restrict BookRead True
route GET Unread         = restrict BookRead False
route GET (Location l)   = restrict BookLocation l
route GET (Category c)   = restrict BookCategory c
route GET (Borrower b)   = restrict BookBorrower b

route GET Add        = add
route GET (Edit e)   = edit e
route GET (Delete d) = delete d

route POST Add        = commitAdd
route POST (Edit e)   = commitEdit e
route POST (Delete d) = commitDelete d

route _ _ = serverError methodNotAllowed405 "Method not allowed"

-------------------------

-- |404 handler.
error404 :: String -> Handler Sitemap
-- Haaacky! Figure out how to handle this in the template haskell
-- code.
error404 "/" = route GET List
error404 uri = serverError notFound404 ("File not found: " <> pack uri)

-- |500 handler.
error500 :: String -> Handler Sitemap
error500 = serverError internalServerError500 . pack

-------------------------

-- |Serve requests
serve :: PathInfo r
      => (StdMethod -> r -> Handler r) -- ^ Routing function
      -> (String -> Handler r) -- ^ 404 handler.
      -> (String -> Handler r) -- ^ 500 handler.
      -> ConfigParser -- ^ The configuration
      -> ConnectionPool -> LoggingT IO ()
serve route on404 on500 conf pool = do
  let host = get conf "host"
  let port = get conf "port"

  let settings = setHost (fromString host) . setPort port $ W.defaultSettings

  logInfoN $ "Listening on " <> pack host <> ":" <> (pack . show) port

  liftIO . runSettings settings $ runner pool

  where
    runner p = handleWai_ toPathInfo' fromPathInfo' (fromString webroot) $ \mkurl ->
      -- Hamlet needs a slightly different @MkUrl@ type than what web-routes-wai gives us.
      let mkurl' r = mkurl (Right r) . map (\(a,b) -> (a, if b == "" then Nothing else Just b))
       in staticPolicy (addBase fileroot) . process p mkurl'

      where
        toPathInfo' (Right p) = toPathInfoParams p

        fromPathInfo' bs = case fromPathInfo bs of
          Right url -> Right (Right url)
          Left  _   -> Right (Left  bs)

        webroot  = get conf "web_root"
        fileroot = get conf "file_root"

    process p mkurl path req receiver = requestHandler `catchIOError` runError
      where
        requestHandler = case path of
          Right uri -> runHandler' $ route method uri
          Left  uri -> runHandler' $ on404 (B8.unpack uri)
        runError err   = runHandler' $ on500 (show err)
        runHandler' h  = runHandler h p mkurl req receiver
        method         = forceEither . parseMethod . requestMethod $ req

    runHandler h p mkurl req receiver = do
      (ps, fs) <- parseRequestBody lbsBackEnd req
      let ps' = map (second $ fromMaybe "") $ queryString req
      let cry = Request
                  { _remoteHost = remoteHost req
                  , _uri    = pack . B8.unpack $ rawPathInfo req
                  , _method = requestMethod req
                  , _params = map (decodeUtf8 *** decodeUtf8) (ps ++ ps')
                  , _files  = map (first decodeUtf8) fs
                  , _conf   = conf
                  , _mkurl  = mkurl
                  }

      (runResourceT . runLogging conf . flip runReaderT cry $ runSqlPool h p) >>= receiver

-------------------------

-- |Run the logging to stderr, cutting off messages below the
-- threshold.
runLogging :: MonadIO m => ConfigParser -> LoggingT m a -> m a
runLogging conf = runStderrLoggingT . filterLog conf

-- |Filter out log messages below the threshold.
filterLog :: ConfigParser -> LoggingT m a -> LoggingT m a
filterLog conf = case get conf "log_level" :: String of
  "2" -> filterLogger $ \_ lvl -> lvl >= LevelWarn
  "1" -> filterLogger $ \_ lvl -> lvl >= LevelInfo
  _   -> filterLogger $ \_ _ -> True
