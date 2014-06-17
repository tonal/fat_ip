-- module Main
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Read as TL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as Encoding

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
-- import Data.Function (on)
import qualified Data.HashMap.Strict as DM
-- import qualified Data.Map.Strict as DM
import Data.List (isSuffixOf, sortBy, foldl',foldr)
-- import Data.Maybe (fromMaybe)
import Data.Hashable
import Data.Ord (comparing)
import Data.Traversable (forM)
import Data.Maybe (mapMaybe)

import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Text.Printf

-- import Debug.Trace

dir :: [String]
dir = ["logs/20140611"]

line2item :: TL.Text -> Maybe (Text, Int)
line2item line = case TL.words line of
  ip : _ : timeStr : _
    | Right (t,"") <- TL.decimal timeStr -> Just (T.copy $ TL.toStrict ip, t)
  _ -> Nothing

main :: IO()
main = do
  args <- getArgs

  let paths = if null args then dir else args
  
  files <- Traversable.forM paths (\path -> do
    fnames <- getDirectoryContents path
    return . map (path </>) $ fnames)

   
  let logs = filter (isSuffixOf "-access_log") . concat $ files
  -- dat <- fmap TL.concat (forM logs (fmap Encoding.decodeLatin1 . BL.readFile))

  items <- unionsWith (+) <$> prepare logs
  
  putStrLn $ "len: " ++ (show $ DM.size items)
  let -- Выбираем 10 самых толстых
      strs = take 10 . sortBy (flip $ comparing snd) . DM.toList $ items
  putStrLn . unlines . map (\(i, t) -> printf "%s: %d" (T.unpack i) t) $ strs

  where 
    prepare = Traversable.traverse (fmap go  . BL.readFile)
    go = DM.fromListWith (+) . mapMaybe line2item . TL.lines . Encoding.decodeLatin1




unionsWith :: (Eq k, Hashable k) => (v -> v -> v) => [DM.HashMap k v] -> DM.HashMap k v
unionsWith _ []  = error "!!!"
unionsWith _ [x] = x
unionsWith f z   = unionsWith f (go z)
   where
     go [] = []
     go [x] = [x]
     go (x:y:z) = DM.unionWith f x y : go z
