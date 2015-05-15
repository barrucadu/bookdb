{-# LANGUAGE FlexibleContexts #-}

-- |Configuration file handling. This module re-exports `ConfgParser`
-- and `get` from `Data.ConfigFile`, as pretty much all applications
-- will want to use those in conjunction with this module.
module Configuration
    ( ConfigParser
    , loadConfigFile
    , reloadConfigFile
    , applyUserConfig
    , defaults
    , get
    , get'
    , conf
    , conf') where

import Control.Monad.Error.Class (MonadError)
import Data.ConfigFile
import Data.Either.Utils (forceEither)
import Server.Requests.Types (RequestProcessor, askConf)
import System.IO.Error (catchIOError)
import Web.Routes.PathInfo (PathInfo)

-- |Load a configuration file by name.
-- All errors (syntax, file access, etc) are squashed together,
-- returning a Nothing if anything fails. This is probably ok, given
-- the simplicity of the format, however it may be useful later on to
-- distinguish between syntax errors (and where they are) and access
-- errors, giving the user a better error message than just "oops,
-- something went wrong"
--
-- String interpolation is turned on (with a depth of 10). See the
-- `Data.ConfigParser` documentation for details on that.
--
-- This sets all of the configuration values expected by the main
-- application to their defaults if they weren't present already, but
-- other values are left as they are in the file.
loadConfigFile :: FilePath -> IO (Maybe ConfigParser)
loadConfigFile filename = (Just <$> loadConfigFileUnsafe filename) `catchIOError` const (return Nothing)

-- |Load a configuration file unsafely. This may throw an IO
-- exception.
loadConfigFileUnsafe :: FilePath -> IO ConfigParser
loadConfigFileUnsafe filename = do
  let base = emptyCP { accessfunc = interpolatingAccess 10 }
  cp <- readfile base filename
  return . merge defaults $ forceEither cp

-- |Reload and reapply the configuration file. If an error is raised,
-- fall back to the original configuration. Specifically, this merges
-- the new configuration with the old, so if the new configuration has
-- removed a required setting, this won't cause a problem.
reloadConfigFile :: ConfigParser    -- ^ The original configuration
                 -> FilePath        -- ^ The config file to load
                 -> IO ConfigParser -- ^ The new configuration
reloadConfigFile cfg filename = ((cfg `merge`) <$> loadConfigFileUnsafe filename) `catchIOError` const (return cfg)

-- |Default configuration values:
--
-- - Listen on *:3000
-- - Use http://localhost:3000 as the basis for all URLs
-- - Use /tmp as the basis for all file look-ups
-- - Use a database called bookdb.sqlite

defaults :: ConfigParser
defaults = forceEither . readstring emptyCP $ unlines
  [ "[bookdb]"
  , "host          = *"
  , "port          = 3000"
  , "web_root      = http://localhost:3000"
  , "file_root     = /tmp"
  , "database_file = bookdb.sqlite"
  ]

-- |Apply the supplied configuration to the standard
-- configuration. This overrides the values in the original
-- configuration with the ones in the user configuration, if provided,
-- preserving any missing values.
applyUserConfig :: ConfigParser       -- ^ The standard configuration
                -> Maybe ConfigParser -- ^ Optional application-specific configuration
                -> ConfigParser
applyUserConfig cfg (Just usercfg) = cfg `merge` usercfg
applyUserConfig cfg _ = cfg

-- |Get a value from the configuration unsafely (throws an
-- `IOException` on fail).
get' :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
get' cp ss os = forceEither $ get cp ss os

-- |Get a value from the configuration in a handler. I found as I was
-- using Seacat that my handlers all started with a block of the form,
--
-- > cfg <- askConf
-- > let foo = get cfg "section" "foo"
-- > let bar = get cfg "section" "bar"
-- > let baz = get cfg "section" "baz"
--
-- This simplifies that by getting rid of the need to use `askConf`
-- manually.
conf :: (Get_C a, MonadError CPError m, PathInfo r) => SectionSpec -> OptionSpec -> RequestProcessor r (m a)
conf ss os = askConf >>= \config -> return $ get config ss os

-- |Get a value from the configuration in a handler unsafely. Like
-- `conf`, but throws an `IOException` if the value can't be found.
conf' :: (Get_C a, PathInfo r) => SectionSpec -> OptionSpec -> RequestProcessor r a
conf' ss os = askConf >>= \config -> return $ get' config ss os
