{-# LANGUAGE OverloadedStrings #-}

-- |Configuration file handling.
module Configuration where

import qualified Data.HashMap.Strict       as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           Database.Selda.PostgreSQL (PGConnectInfo(..))
import           System.IO.Error           (catchIOError, ioError, userError)
import           Text.Parsec.Error         (errorMessages, messageString)
import           Text.Toml
import           Text.Toml.Types

-- | The configuration record.
data Configuration = Configuration
  { cfgHost     :: String
  , cfgPort     :: Int
  , cfgWebRoot  :: String
  , cfgFileRoot :: String
  , cfgDatabase :: PGConnectInfo
  , cfgReadOnly :: Bool
  }

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
    { cfgHost = maybe "*" T.unpack (getStr "host")
    , cfgPort = fromMaybe 3000 (getInt "port")
    , cfgWebRoot = maybe "http://localhost:3000" T.unpack (getStr "web_root")
    , cfgFileRoot = maybe "/tmp" T.unpack (getStr "file_root")
    , cfgDatabase = PGConnectInfo
      { pgHost = fromMaybe "localhost" (getStr "pg_host")
      , pgPort = fromMaybe 5432 (getInt "pg_port")
      , pgDatabase = fromMaybe "bookdb" (getStr "pg_name")
      , pgSchema = getStr "pg_schema"
      , pgUsername = getStr "pg_username"
      , pgPassword = getStr "pg_password"
      }
    , cfgReadOnly = fromMaybe False (getBool "read_only")
    }
  where
    getInt key = case M.lookup key =<< tbl of
      Just (VInteger val) -> Just (fromIntegral val)
      _ -> Nothing
    getStr key = case M.lookup key =<< tbl of
      Just (VString val) -> Just val
      _  -> Nothing
    getBool key = case M.lookup key =<< tbl of
      Just (VBoolean val) -> Just val
      _  -> Nothing
