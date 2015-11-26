{-# LANGUAGE FlexibleContexts #-}

-- |Configuration file handling. This module re-exports `ConfgParser`
-- and `get` from `Data.ConfigFile`, as pretty much all applications
-- will want to use those in conjunction with this module.
module Configuration
    ( ConfigParser
    , loadConfigFile
    , defaults
    , get
    , conf) where

import Data.ConfigFile hiding (get)
import Data.Either.Utils (forceEither)
import System.IO.Error (catchIOError)

import Requests

import qualified Data.ConfigFile as C

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
  , "readonly      = false"
  , "log_level     = 1"
  ]

-- |Get a value from the configuration, throwing an 'IOException' if
-- the value can't be found.
get :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
get cp ss os = forceEither $ C.get cp ss os

-- |Get a value from the configuration in a handler, throwing an
-- 'IOException' if the value can't be found.
conf :: Get_C a => SectionSpec -> OptionSpec -> RequestProcessor r a
conf ss os = askConf >>= \config -> return $ get config ss os
