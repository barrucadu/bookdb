module Handler.Utils (suggest) where

import Database
import Routes

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Web.Seacat
import System.Random (randomRIO)

-- |Get a suggestion
suggest :: RequestProcessor Sitemap (Maybe Book)
suggest = do
  books <- selectList [BookRead ==. False] []

  idx <- liftIO $ randomRIO (0, length books)

  if null books
  then return Nothing
  else return . Just . (\(Entity _ e) -> e) $ books !! idx
