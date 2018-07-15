module Handler.Stats (stats) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Function (on)
import Data.List (foldl', genericLength)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Database
import Requests
import Routes

import qualified Handler.Templates as T

stats :: Handler Sitemap
stats = do
  now <- liftIO getCurrentTime
  let ago = addUTCTime (-31536000) now

  lastYearBooks <- lift $ readSince ago
  leastRecentBooks <- lift $ leastRecent
  let readDates = mapMaybe bookLastRead leastRecentBooks

  let byYear  = groupBy' eqYear readDates
  let byMonth = sortMonths readDates

  let thisYear = map (genericLength . filter (eqYear now)) byMonth
  let lastYear = map (genericLength . filter (eqYear ago)) byMonth
  let avgYear  = map ((/genericLength byYear) . genericLength) byMonth

  htmlUrlResponse $ T.stats lastYearBooks leastRecentBooks thisYear lastYear avgYear

-- | Check if two timestamps are from the same year
eqYear :: UTCTime -> UTCTime -> Bool
eqYear = (==) `on` formatTime defaultTimeLocale "%Y"

-- | @groupBy@ which groups non-adjacent things.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq = foldl' go [] where
  go [] x = [[x]]
  go (ys@(y:_):yss) x
    | x `eq` y  = (x:ys) : yss
    | otherwise = ys : go yss x

-- | Group a list of month data by month.
sortMonths :: [UTCTime] -> [[UTCTime]]
sortMonths ds = map inMonth [1..12] where
  inMonth m
    | m < 10    = filter (\d -> formatTime defaultTimeLocale "%m" d == '0':show m) ds
    | otherwise = filter (\d -> formatTime defaultTimeLocale "%m" d == show m)     ds
