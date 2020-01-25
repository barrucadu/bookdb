{-# LANGUAGE OverloadedStrings #-}

-- |Configuration file handling.
module Configuration where

import           Control.Monad             (guard)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           Database.Selda.PostgreSQL (PGConnectInfo(..))
import           System.Environment        (lookupEnv)
import           Text.Read                 (readMaybe)

-- | The configuration record.
data Configuration = Configuration
  { cfgHost     :: String
  , cfgPort     :: Int
  , cfgWebRoot  :: String
  , cfgFileRoot :: String
  , cfgDatabase :: PGConnectInfo
  , cfgReadOnly :: Bool
  }

-- | Read configuration from the environment.
getConfig :: IO (Either [String] Configuration)
getConfig = do
  bookdb_host <- lookupEnv "BOOKDB_HOST"
  bookdb_port <- lookupEnv "BOOKDB_PORT"
  bookdb_web_root <- lookupEnv "BOOKDB_WEB_ROOT"
  bookdb_file_root <- lookupEnv "BOOKDB_FILE_ROOT"
  bookdb_pg_host <- lookupEnv "BOOKDB_PG_HOST"
  bookdb_pg_port <- lookupEnv "BOOKDB_PG_PORT"
  bookdb_pg_db <- lookupEnv "BOOKDB_PG_DB"
  bookdb_pg_schema <- lookupEnv "BOOKDB_PG_SCHEMA"
  bookdb_pg_username <- lookupEnv "BOOKDB_PG_USERNAME"
  bookdb_pg_password <- lookupEnv "BOOKDB_PG_PASSWORD"
  bookdb_read_only <- lookupEnv "BOOKDB_READ_ONLY"

  pure . runValidation $ Configuration
    <$> pure (fromMaybe "*" bookdb_host)
    <*> maybeToValidation "could not parse BOOKDB_PORT (expected integer in range 0..65535)" (maybe (Just 3000) readPort bookdb_port)
    <*> pure (fromMaybe "http://localhost:3000" bookdb_web_root)
    <*> pure (fromMaybe "/tmp" bookdb_file_root)
    <*> (PGConnectInfo
         <$> pure (maybe "localhost" T.pack bookdb_pg_host)
         <*> maybeToValidation "could not parse BOOKDB_PG_PORT (expected integer in range 0..65535)" (maybe (Just 5432) readPort bookdb_pg_port)
         <*> pure (maybe "bookdb" T.pack bookdb_pg_db)
         <*> pure (T.pack <$> bookdb_pg_schema)
         <*> pure (T.pack <$> bookdb_pg_username)
         <*> pure (T.pack <$> bookdb_pg_password)
        )
    <*> maybeToValidation "could not parse BOOKDB_READ_ONLY (expected boolean)" (maybe (Just False) readBool bookdb_read_only)

-- | Like @Either [e] a@, but accumulates errors.
newtype Validation e a = Validation { runValidation :: Either [e] a }

instance Functor (Validation e) where
  fmap f (Validation e) = Validation (fmap f e)

instance Applicative (Validation e) where
  pure a = Validation (Right a)
  Validation (Right f) <*> Validation (Right a) = Validation (Right (f a))
  Validation (Left el) <*> Validation (Left er) = Validation (Left (el ++ er))
  Validation (Left el) <*> _ = Validation (Left el)
  _ <*> Validation (Left er) = Validation (Left er)

-- | Convert a Maybe into a Validation
maybeToValidation :: Read a => e -> Maybe a -> Validation e a
maybeToValidation e = Validation . maybe (Left [e]) Right

-- | Parse a port number.
readPort :: String -> Maybe Int
readPort str = do
  p <- readMaybe str
  guard (p > 0)
  guard (p < 65536)
  pure p

-- | Parse a boolean.
readBool :: String -> Maybe Bool
readBool str
  | str `elem` ["true", "True", "TRUE"] = Just True
  | str `elem` ["false", "False", "FALSE"] = Just False
  | otherwise = Nothing
