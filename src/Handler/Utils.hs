module Handler.Utils (suggest) where

import Control.Monad.IO.Class (liftIO)
import Database
import Database.Persist
import Routes
import System.Random (randomRIO)
import Web.Seacat

-- |Get a suggestion
suggest :: RequestProcessor Sitemap (Maybe Book)
suggest = do
  books <- selectList [BookRead ==. False] []

  idx <- liftIO $ randomRIO (0, length books)

  if null books
  then return Nothing
  else return . Just . (\(Entity _ e) -> e) $ books !! idx
