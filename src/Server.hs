{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Here lives everything to do with starting up a Seacat server.
module Server
    ( -- * Types from dependencies, re-exported for convenience.
      FileInfo(..)
    , PathInfo(..)
    , ConfigParser

    -- * Running the server
    , SeacatSettings(..)
    , defaultSettings
    , seacat

    -- * Request handler types
    , Cry(..)
    , MkUrl
    , RequestProcessor
    , Handler

    -- * Request handler utilities
    , askCry
    , askConf
    , askMkUrl
    , askReq

    -- * Response builders
    , htmlResponse
    , htmlResponse'
    , textResponse
    , textResponse'
    , respondFile
    , redirect

    -- * Parameter accessors
    , param
    , param'
    , hasParam
    , params

    -- * File upload handling
    , files
    , save
    , save'
    ) where

import Configuration (ConfigParser, applyUserConfig, defaults, get', loadConfigFile, reloadConfigFile)
import Control.Arrow ((***), first, second)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Data.Text (replace)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sql (ConnectionPool, Migration, SqlPersistM, runMigration, runSqlPool, runSqlPersistMPool)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.Wai (Application, requestMethod, queryString)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort)
import Network.Wai.Middleware.Gzip (GzipSettings(..), GzipFiles(GzipCompress), gzip, gzipFiles, def)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Server.Requests
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (catchIOError)
import Web.Routes.Wai (handleWai)

import qualified Network.Wai.Handler.Warp as W

-- |Optional configuration for Seacat servers
data SeacatSettings = SeacatSettings
    { _config   :: Maybe ConfigParser
      -- ^ Default configuration, overriding the defaults. This should
      -- include all application-required configuration not already
      -- provided by Seacat. Configuration is applied as follows,
      -- where \`merge\` overrides the values in its first argument by
      -- the values in its second,
      --
      --     seacat defaults \`merge\` application config \`merge\` user config
      --
      -- This ensures that all the required configuration values are set.

    , _migrate  :: Maybe Migration
      -- ^ Database migration handler. If a database is used, this must
      -- be provided (or migrations handled manually in runserver).

    , _populate :: Maybe (SqlPersistM ())
      -- ^ Database population handler. If a database is used, this
      -- must be provided (or population handled manually in
      -- runserver).

    , _clean :: Maybe (SqlPersistM ())
      -- ^ Database clean handler. This is optional.

    , _gzip :: GzipSettings
      -- ^ The settings to use for Gzip compression.
    }

-- |Default configuration: no application-specific configuration, no
-- migration handler, no population handler, and gzip if the browser
-- accepts it.
defaultSettings :: SeacatSettings
defaultSettings = SeacatSettings { _config   = Nothing
                                 , _migrate  = Nothing
                                 , _populate = Nothing
                                 , _clean    = Nothing
                                 , _gzip     = def { gzipFiles = GzipCompress }
                                 }

-- |Launch the Seacat web server. Seacat takes two bits of mandatory
-- configuration, a routing function and a 500 handler, and then some
-- optional configuration. By default, the server listens on *:3000.
seacat :: PathInfo r
       => (StdMethod -> r -> Handler r) -- ^ Routing function
       -> (String -> Handler r) -- ^ Top-level error handling function
       -> SeacatSettings -- ^ Optional configuration
       -> IO ()
seacat route on500 settings = do
  args <- getArgs

  when (length args < 1) $
    die "Expected at least one argument"

  let command = head args

  let confFile = case args of
                   (_:conffile:_) -> Just conffile
                   _ -> Nothing

  config <- case confFile of
             Just cfile -> loadConfigFile cfile
             Nothing -> return $ Just defaults

  case config of
    Just conf -> let conf'    = applyUserConfig conf (_config settings)
                     connstr = get' conf' "bookdb" "database_file" 

                     pool = withSqlitePool (fromString connstr) 10

                     settings' = settings { _config = Just conf' }
                 in run command route on500 confFile pool settings'
    Nothing   -> die "Failed to read configuration"

-- |Die with a fatal error
die :: String -- ^ The error description
    -> IO ()
die err = putStrLn err >> exitFailure

-------------------------

-- |Run one of the applications, depending on the command
run :: PathInfo r
    => String -- ^ Command
    -> (StdMethod -> r -> Handler r) -- ^ Routing function
    -> (String -> Handler r) -- ^ Top-level error handling function
    -> Maybe FilePath -- ^ The config file
    -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ()) -- ^ Database connection pool runner
    -> SeacatSettings -- ^ The optional settings
    -> IO ()
run "runserver" = runserver
run "migrate"   = migrate
run "populate"  = populate
run "clean"     = clean
run _           = badcommand

-- |Run the server
runserver :: PathInfo r
          => (StdMethod -> r -> Handler r)
          -> (String -> Handler r)
          -> Maybe FilePath
          -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ())
          -> SeacatSettings
          -> IO ()
runserver route on500 cfile pool settings = do
  let conf = fromJust $ _config settings

  let host = get' conf "bookdb" "host"
  let port = get' conf "bookdb" "port"

  void $ clean route on500 cfile pool settings

  let settings' = setHost (fromString host) . setPort port $ W.defaultSettings

  putStrLn $ "Listening on " ++ host ++ ":" ++ show port
  runNoLoggingT . pool $ liftIO . runSettings settings' . runner settings route on500 (conf,cfile)

-- |Migrate the database
migrate :: PathInfo r
        => (StdMethod -> r -> Handler r)
        -> (String -> Handler r)
        -> Maybe FilePath
        -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ())
        -> SeacatSettings
        -> IO ()
migrate _ _ _ pool settings = do
  case _migrate settings of
    Just migration -> runNoLoggingT . runDB pool $ runMigration migration
    Nothing -> putStrLn "No application migration handler."

-- |Populate the database with test data
populate :: PathInfo r
         => (StdMethod -> r -> Handler r)
         -> (String -> Handler r)
         -> Maybe FilePath
         -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ())
         -> SeacatSettings
         -> IO ()
populate _ _ _ pool settings =
    case _populate settings of
      Just pop -> runNoLoggingT $ runDB pool pop
      Nothing -> die "No population handler."

-- |Clean out expired bans from the database
clean :: PathInfo r
      => (StdMethod -> r -> Handler r)
      -> (String -> Handler r)
      -> Maybe FilePath
      -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ())
      -> SeacatSettings
      -> IO ()
clean _ _ _ pool settings = do
  now <- getCurrentTime

  case _clean settings of
    Just cln -> runNoLoggingT $ runDB pool cln
    Nothing -> putStrLn "No application clean handler."

-- |Fail with an error
badcommand :: PathInfo r
           => (StdMethod -> r -> Handler r)
           -> (String -> Handler r)
           -> Maybe FilePath
           -> ((ConnectionPool -> NoLoggingT IO ()) -> NoLoggingT IO ())
           -> SeacatSettings
           -> IO ()
badcommand _ _ _ _ _ = die "Unknown command"


-- |Run a database function with a connection pool
runDB :: (MonadIO m, MonadBaseControl IO m)
      => ((ConnectionPool -> m a) -> m a) -- ^ The connection pool
      -> SqlPersistM a -- ^ The database function
      -> m a
runDB pool f = pool $ liftIO . runSqlPersistMPool f

-------------------------

-- |runner is the actual WAI application. It takes a request, handles
-- it, and produces a response.
runner :: PathInfo r
       => SeacatSettings -- ^ The settings
       -> (StdMethod -> r -> Handler r)    -- ^ Routing function
       -> (String -> Handler r)           -- ^ Top-level error handling function
       -> (ConfigParser, Maybe FilePath) -- ^ The configuration
       -> ConnectionPool                 -- ^ Database connection reference
       -> Application
runner settings route on500 c@(conf,_) pool = handleWai (fromString webroot) $ \mkurl r ->
  -- This is horrific, come up with a better way of doing it
  let mkurl' r' args = replace "%23" "#" . mkurl r' $ map (\(a,b) -> (a, if b == "" then Nothing else Just b)) args
  in staticPolicy (addBase fileroot) $
     gzip (_gzip settings) $
     process route on500 c pool mkurl' r

  where webroot  = get' conf "bookdb" "web_root"
        fileroot = get' conf "bookdb" "file_root"

-- |Route and process a request
-- Todo: use SqlPersistT?
process :: PathInfo r
        => (StdMethod -> r -> Handler r) -- ^ Routing function
        -> (String -> Handler r)        -- ^ Top-level error handling function
        -> (ConfigParser, Maybe FilePath) -- ^ The configuration
        -> ConnectionPool              -- ^ Database connection reference
        -> MkUrl r                     -- ^ URL building function
        -> r                           -- ^ Requested route
        -> Application
process route on500 (conf,cfile) pool mkurl path req receiver = requestHandler `catchIOError` runError
  where requestHandler = runHandler' $ route method path
        runError err   = runHandler' $ on500 (show err)
        runHandler' h  = runHandler h conf cfile pool mkurl req receiver 
        method         = forceEither . parseMethod . requestMethod $ req

-- |Run a request handler.
runHandler :: PathInfo r
           => Handler r -- ^ The handler to run
           -> ConfigParser
           -> Maybe FilePath
           -> ConnectionPool
           -> MkUrl r
           -> Application
runHandler h conf cfile pool mkurl req receiver = do
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

  (runResourceT . runNoLoggingT . flip runReaderT cry $ runSqlPool h pool) >>= receiver
