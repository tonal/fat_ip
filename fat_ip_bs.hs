-- module Main
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

-- import Data.Function (on)
import qualified Data.HashMap.Strict as DM
-- import qualified Data.Map.Strict as DM
import Data.List (isSuffixOf, sortBy, foldl')
-- import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (forM)

import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Text.Printf

-- import Debug.Trace

dir :: String
dir = "logs/20140611"

line2item :: BSL.ByteString -> Maybe (BS.ByteString, Int)
line2item line = case BSL.words line of
  ip : _ : timeStr : _
    | Just (t, "") <- BSL.readInt timeStr -> Just (BS.copy $ BSL.toStrict ip, t)
  _ -> Nothing

main :: IO()
main = do
  args <- getArgs
  let paths = if null args then [dir] else args
  files <- forM paths (\path -> do
    fnames <- getDirectoryContents path
    return . map (path </>) $ fnames)
  let logs = filter (isSuffixOf "-access_log") . concat $ files
  conts <- forM logs BSL.readFile -- Вычитываем файлы
  let log_contents = map (map line2item . BSL.lines) conts
  let items = foldl' f DM.empty . concat $ log_contents
      f ips Nothing = ips
      f ips (Just (ip, time)) = DM.insertWith (+) ip time ips
  putStrLn $ "len: " ++ (show $ DM.size items)
  let -- Выбираем 10 самых толстых
      strs = take 10 . sortBy (flip $ comparing snd) . DM.toList $ items
  putStrLn . unlines . map (\(i, t) -> printf "%s: %d" (BS.unpack i) t) $ strs
