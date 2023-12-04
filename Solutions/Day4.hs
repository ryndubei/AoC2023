module Solutions.Day4 (solution1, solution2) where

import Data.List (intersect)
import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.MemoTrie (memo2)

solution1 :: String -> Int
solution1 s = sum $ do
  l <- lines s
  let l' = drop 10 l
      (wins, haves) = second tail $ break (=='|') l'
      wins' = map read $ words wins :: [Int]
      haves' = map read $ words haves :: [Int]
      matches = length $ intersect wins' haves'
  pure $ case matches of
    0 -> 0
    n -> 2^(n - 1)

solution2 :: String -> Int
solution2 s = sum $ do
  (i,l) <- Map.toList cards
  go' i l
  where
    cards = cardMap s
    go' = memo2 go
    go i l = do
      let (wins, haves) = second tail $ break (=='|') l
          wins' = map read $ words wins :: [Int]
          haves' = map read $ words haves :: [Int]
          matches = length $ wins' `intersect` haves'
      1 : case matches of
        0 -> []
        _ -> do
          (i', l') <- [ (idx, fromJust $ Map.lookup idx cards) | idx <- [i+1..i+matches]]
          go' i' l'

cardMap :: String -> Map Int String
cardMap s = Map.fromList $ do
  l <- lines s
  let i = read . takeWhile (/=':') $ drop 5 l :: Int
  pure (i, drop 10 l)
