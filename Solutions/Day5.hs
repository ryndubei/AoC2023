{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solutions.Day5 (solution1, solution2) where

import Data.Foldable
import Data.Function
import Data.Maybe
import Text.Read (readMaybe)
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import Control.Applicative

solution1 :: String -> Int
solution1 s = minimum $ do
  seed <- seeds s
  let f = (\fs' n -> fromMaybe n . asum $ map ($ n) fs') 
        . map (uncurry offsetPoint) 
        . Map.toList
  pure $ foldl' (&) seed (map f $ maps s)

solution2 :: String -> Int
solution2 s = minimum . map start $ do
  sr <- seedRanges
  foldl' (&) [sr] $ do
    m <- maps'
    pure $ \rs -> do
      r <- rs
      mapRange m r
  where
    maps' = maps s
    seedRanges = map (\case [a,b] -> Range a b) . chunksOf 2 $ seeds s

type Range = Arg Int Int

pattern Range :: Int -> Int -> Range
pattern Range{start, len} = (Arg start len)
{-# COMPLETE Range #-}

rangeEnd :: Range -> Int
rangeEnd Range{start, len} = start + len - 1

seeds :: String -> [Int]
seeds = map read . words . drop 6 . head . lines

maps :: String -> [Map Range Int]
maps s = map f mapNames
  where
    s' = tail s
    f name =
      let ls = tail . dropWhile (/= name) $ lines s'
          ls' = ls & map \l -> case map readMaybe $ words l of
            [Just a, Just b, Just c] -> Just (Range b c, a - b)
            _ -> Nothing
       in Map.fromList . catMaybes $ takeWhile isJust ls'
    mapNames = map (++ " map:")
      [ "seed-to-soil"
      , "soil-to-fertilizer"
      , "fertilizer-to-water"
      , "water-to-light"
      , "light-to-temperature"
      , "temperature-to-humidity"
      , "humidity-to-location"
      ]

offsetPoint :: Range -> Int -> Int -> Maybe Int
offsetPoint Range{start, len} offset n
  | inRange = Just $ n + offset
  | otherwise = Nothing
  where
    inRange = not (n < start || n >= start + len)

-- | Ranges (r1, r2) s.t. r1 + r2 = r, all elements in r1 < i and
-- all elements in r2 >= i
partitionRange :: Range -> Int -> (Range, Range)
partitionRange r@Range{start, len} i
  | i < start = (Range start 0, r)
  | i > rangeEnd r = (r, Range (rangeEnd r) 0)
  | otherwise = let i' = i - start
                 in (Range start i', Range i (len - i'))

-- | @slice r1 r2@ returns @(a, b, c)@ such that @b `subrange` r2@,
-- @a + b + c = r1@ and @a,c,r2@ are disjoint. 
-- @a,c@ may have a length of 0.
slice :: Range -> Range -> (Range, Range, Range)
slice r (Range s2 l2) =
  let (lrem, r') = partitionRange r s2
      (r'', rrem) = partitionRange r' (s2 + l2)
   in (lrem, r'', rrem)

offsetRange :: Range -> Int -> Range
offsetRange r o = r{start = start r + o}

mapRange :: Map Range Int -> Range -> [Range]
mapRange _ (Range _ 0) = []
mapRange ranges sr =
  case Map.lookupLE sr ranges <|> Map.lookupGT sr ranges of
    Nothing -> [sr]
    Just (r,o) ->
      let (lrem, sr', rrem) = slice sr r
          (lranges,rranges) = Map.split sr ranges
       in mapRange lranges lrem
          ++ [offsetRange sr' o | len sr' /= 0]
          ++ mapRange rranges rrem
