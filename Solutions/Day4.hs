module Solutions.Day4 (solution1, solution2) where

import Data.List (intersect)
import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.MemoTrie (memo2)

solution1 :: String -> Int
solution1 s = sum $ do
  (_,matches) <- Map.toList $ cardMap s
  pure $ case matches of
    0 -> 0
    n -> 2^(n - 1)

solution2 :: String -> Int
solution2 s = sum $ do
  (i,matches) <- Map.toList cards
  go' i matches
  where
    cards = cardMap s
    go' = memo2 go
    go i matches = 1 : do
      i' <- [i+1..i+matches]
      let matches' = fromJust $ Map.lookup i' cards
      go' i' matches'

cardMap :: String -> Map Int Int
cardMap s = Map.fromList $ do
  l <- lines s
  let i = read . takeWhile (/=':') $ drop 5 l
      l' = drop 10 l
      (wins, haves) = second tail $ break (=='|') l'
      wins' = words wins
      haves' = words haves
      matches = length $ wins' `intersect` haves'
  pure (i, matches)
