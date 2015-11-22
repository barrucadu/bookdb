module Handler.Stats (stats) where

import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (foldl', genericLength)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.Esqueleto hiding (on)

import Database
import Handler.Utils
import Requests
import Routes

import qualified Handler.Templates as T

stats :: Handler Sitemap
stats = do
  now <- liftIO getCurrentTime
  let ago = addUTCTime (-31536000) now

  lastYearBooks <- fmap unEntities . select $
                  from $ \b -> do
                    where_ (b ^. BookRead)
                    where_ (b ^. BookLastread >=. just (val ago))
                    orderBy [desc (b ^. BookLastread), rand]
                    return b

  leastRecentBooks <- fmap unEntities . select $
                     from $ \b -> do
                       where_ (b ^. BookRead)
                       orderBy [asc (b ^. BookLastread), rand]
                       limit 50
                       return b

  readDates <- fmap (catMaybes . unValues) . select $
              from $ \b -> do
                orderBy [desc (b ^. BookLastread)]
                return $ b ^. BookLastread

  let byYear  = groupBy' eqYear  readDates :: [[UTCTime]]
  let byMonth = groupBy' eqMonth readDates :: [[UTCTime]]

  let thisYear = map (length . filter (eqYear now)) byMonth
  let lastYear = map (length . filter (eqYear ago)) byMonth
  let avgYear  = map ((/genericLength byYear) . genericLength) byMonth

  htmlUrlResponse $ T.stats lastYearBooks leastRecentBooks thisYear lastYear avgYear

-- | Check if two timestamps are from the same year
eqYear :: UTCTime -> UTCTime -> Bool
eqYear = (==) `on` formatTime defaultTimeLocale "%Y"

-- | Check if two timestamps are from the same month
eqMonth :: UTCTime -> UTCTime -> Bool
eqMonth = (==) `on` formatTime defaultTimeLocale "%m"

-- | @groupBy@ which groups non-adjacent things.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq = foldl' go [] where
  go [] x = [[x]]
  go (ys@(y:_):yss) x
    | x `eq` y  = (x:ys) : yss
    | otherwise = ys : go yss x
