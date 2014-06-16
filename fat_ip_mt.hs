{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- import Control.Applicative
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Read as L
import qualified Data.Map as Map
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Traversable (forM)

import System.Environment (getArgs)
import System.Directory
import System.FilePath ((</>))

dir :: String
dir = "logs/20140611"

line2item :: Text -> Maybe (Text, Int)
line2item line = case L.words line of
  ip : _ : timeStr : _
    | Right (t,"") <- L.decimal timeStr -> Just (ip, t)
  _ -> Nothing

fileItems :: Text -> [(Text,Int)]
fileItems = catMaybes . map line2item . L.lines

stats :: [(Text,Int)] -> [(Text,Int)]
stats
  = take 10
  . sortBy (flip $ comparing snd)
  . Map.toList
  . foldl' (\m (ip,t) -> Map.insertWith' (+) ip t m) Map.empty

main :: IO ()
main = do
  args <- getArgs
  let paths = if null args then [dir] else args
  files <- forM paths (\path -> do
    fnames <- getDirectoryContents path
    return . map (path </>) $ fnames)
  let logs = filter (isSuffixOf "-access_log") . concat $ files
  logContents <- mapM (fmap fileItems . L.readFile) logs
  mapM_ (putStrLn . show) $ stats $ concat logContents
