-- module Main
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- import Control.Applicative ((<$>),(<*>),(<|>))
import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

-- import Data.Fixed (Pico)
import Data.Fixed ()
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

import Data.Traversable (forM)

import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

-- import Debug.Trace

import Data.Attoparsec.Char8 as Atto

dir :: String
dir = "logs/20140611"

months :: DM.Map BS.ByteString Int
months = DM.fromList [
  ("Jan", 1), ("Feb", 2), ("Mar", 3), ("Apr", 4), ("May", 5), ("Jun", 6),
  ("Jul", 7), ("Aug", 8), ("Sep", 9), ("Oct", 10), ("Nov", 11), ("Dec", 12)]

lookupMonth :: BS.ByteString -> Maybe Int
lookupMonth = flip DM.lookup months

-- | Parse a given number of digits
nDigit :: (Read a, Num a) => Int -> Parser a
nDigit n = read <$> count n digit

localTime :: Parser LocalTime
localTime = do
  -- 03/May/2014:05:30:02
  d <- date
  t <- char ':' >> time
  return $ LocalTime d t
  where
    time = do
      h <- nDigit 2
      m <- char ':' >> nDigit 2
      s <- char ':' >> nDigit 2
      return $ TimeOfDay h m $ fromInteger s
    date = do
      d <- nDigit 2
      m <- char '/' >> Atto.takeWhile isAlpha_ascii
      y <- char '/' >> nDigit 4
      let m' = fromMaybe (error "invalid month") $ lookupMonth m
      return $ fromGregorian y m' d

line2item :: BS.ByteString -> (LocalTime, BS.ByteString)
line2item line = (fromMaybe (error "invalid datetime") dt, line)
  where
    ldt = BS.takeWhile (/=' ') . BS.tail . BS.dropWhile (/='[') $ line
    dt = maybeResult . parse localTime $ ldt

merge :: forall a a1. Ord a => [(a1, [a])] -> [a]
merge [] = []
merge [x] = snd x
merge xs = iter init_set
  where
    not_null = not . null
    lst2pair [] = error "null list in not nulls"
    lst2pair (e:es) = (e, es)
    init_set = DS.fromList . map lst2pair . filter not_null . map snd $ xs
    iter set
      | DS.null set = []
      | DS.size set == 1 =
        let elt = head . DS.elems $ set
        in fst elt : snd elt
      | otherwise =
        let
          (em, st) = DS.deleteFindMin set
          etail = snd em
          new_set = if null etail then st else DS.insert (lst2pair etail) st
        in fst em : iter new_set

main :: IO()
main = do
  args <- getArgs
  let path = if null args then dir else head args
  files <- getDirectoryContents path
  let logs = filter (isSuffixOf "-access_log") files
  -- putStrLn . unlines $ logs
  log_contents <- forM logs (\fn -> do
    cont <- BSL.readFile $ path </> fn
    return (fn, map line2item . map BSL.toStrict . BSL.lines $ cont))
  let lst_items = merge log_contents
  BSL.putStrLn . BSL.unlines . map BSL.fromStrict . map (\(_, l) -> l) $ lst_items
