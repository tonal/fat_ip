-- module Main
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

-- import Data.Function (on)
import           Data.Hashable
--import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as DM
-- import qualified Data.Map.Strict as DM
import Data.List (isSuffixOf, sortBy, foldl')
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Traversable (forM)
import qualified Data.Traversable as Traversable

import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Text.Printf

-- import Debug.Trace

dir :: String
dir = "logs/20140611"

line2item :: BSL.ByteString -> Maybe (Text, Int)
line2item line = case BSL.words line of
  ip : _ : timeStr : _
    | Just (t, "") <- BSL.readInt timeStr -> Just (decodeLatin1 $ BSL.toStrict ip, t)
  _ -> Nothing

main :: IO()
main = do
  args <- getArgs

  let paths = if null args then [dir] else args

  files <- forM paths (\path -> map (path </>) <$> getDirectoryContents path)

  items <- fmap (unionsWith (+))
         $ Traversable.traverse (fmap (DM.fromListWith (+) . mapMaybe line2item . BSL.lines) .  BSL.readFile)
	 $ filter (isSuffixOf "-access_log")
	 $ concat files
  
  putStrLn $ "len: " ++ (show $ DM.size items)
  let -- Выбираем 10 самых толстых
      strs = take 10 . sortBy (flip $ comparing snd) . DM.toList $ items
  putStrLn . unlines . map (\(i, t) -> printf "%s: %d" (T.unpack i) t) $ strs

unionsWith :: (Eq k, Hashable k) => (v -> v -> v) => [DM.HashMap k v] -> DM.HashMap k v
unionsWith _ []  = error "!!!"
unionsWith _ [x] = x
unionsWith f z   = unionsWith f (go z)
   where
     go [] = []
     go [x] = [x]
     go (x:y:z) = DM.unionWith f x y : go z
