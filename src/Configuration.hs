{-# LANGUAGE OverloadedStrings #-}

-- |Configuration file handling.
module Configuration where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import System.IO.Error (catchIOError, ioError, userError)
import Text.Parsec.Error (errorMessages, messageString)
import Text.Toml
import Text.Toml.Types

-- | The configuration record.
data Configuration = Configuration
  { cfgHost :: String
  , cfgPort :: Int
  , cfgWebRoot :: String
  , cfgFileRoot :: String
  , cfgDatabaseFile :: String
  , cfgReadOnly :: Bool
  } deriving Show

-- | Default configuration.
defaults :: Configuration
defaults = toConfiguration Nothing

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
loadConfigFile :: FilePath -> IO (Maybe Configuration)
loadConfigFile filename = (Just <$> loadConfigFileUnsafe filename) `catchIOError` const (return Nothing)

-- |Load a configuration file unsafely. This may throw an IO
-- exception.
loadConfigFileUnsafe :: FilePath -> IO Configuration
loadConfigFileUnsafe filename = do
  cfg <- readFile filename
  let config = parseTomlDoc filename (T.pack cfg)
  let formatError = unlines . map messageString . errorMessages
  either (ioError . userError . formatError) (pure . toConfiguration . Just) config

-- | Convert a TOML 'Table' to a 'Coniguration' value, filling in the
-- blanks with default values.
toConfiguration :: Maybe Table -> Configuration
toConfiguration tbl = Configuration
  { cfgHost = case M.lookup "host" =<< tbl of
      Just (VString val) -> T.unpack val
      _ -> "*"
  , cfgPort = case M.lookup "port" =<< tbl of
      Just (VInteger val) -> fromIntegral val
      _ -> 3000
  , cfgWebRoot = case M.lookup "web_root" =<< tbl of
      Just (VString val) -> T.unpack val
      _ -> "http://localhost:3000"
  , cfgFileRoot = case M.lookup "file_root" =<< tbl of
      Just (VString val) -> T.unpack val
      _ -> "/tmp"
  , cfgDatabaseFile = case M.lookup "database_file" =<< tbl of
      Just (VString val) -> T.unpack val
      _ -> "bookdb.sqlite"
  , cfgReadOnly = case M.lookup "read_only" =<< tbl of
      Just (VBoolean val) -> val
      _ -> False
  }
