-- module Main
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Read as TL

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

line2item :: TL.Text -> Maybe (TL.Text, Int)
line2item line = case TL.words line of
  ip : _ : timeStr : _
    | Right (t,"") <- TL.decimal timeStr -> Just (ip, t)
  _ -> Nothing

main :: IO()
main = do
  args <- getArgs
  let paths = if null args then [dir] else args
  files <- forM paths (\path -> do
    fnames <- getDirectoryContents path
    return . map (path </>) $ fnames)
  let logs = filter (isSuffixOf "-access_log") . concat $ files
  log_contents <- forM logs (\fn -> do
    cont <- TL.readFile fn
    -- return . map line2item . map BSL.toStrict . BSL.lines $ cont)
    return . map line2item . TL.lines $ cont)
  let items = foldl' f DM.empty . concat $ log_contents
      f ips Nothing = ips
      f ips (Just (ip, time)) = DM.insertWith (+) ip time ips
  {-let its = map (foldl' f DM.empty) log_contents
        items = foldl' (DM.unionWith (+)) DM.empty its-}
  putStrLn $ "len: " ++ (show $ DM.size items)
  let -- Выбираем 10 самых толстых
      strs = take 10 . sortBy (flip $ comparing snd) . DM.toList $ items
  putStrLn . unlines . map (\(i, t) -> printf "%s: %d" (TL.unpack i) t) $ strs
