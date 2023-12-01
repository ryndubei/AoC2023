module Solutions.Day1 (solution1, solution2) where

import Data.List (inits, tails)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

solution1 :: String -> Int
solution1 s = sum $ do
  l <- lines s
  let ns = map (read . pure) $ filter isDigit l
  pure $ (head ns * 10) + last ns

solution2 :: String -> Int
solution2 s = sum $ do
  l <- lines s
  let ns = getNumbers l
  pure $ head ns * 10 + last ns

getNumbers :: String -> [Int]
getNumbers s =
  let s' = tails s >>= inits
   in mapMaybe (`lookup` digits) s'

digits :: [(String, Int)]
digits =
  zip ds [0..] <> zip (map show [0..9 :: Int]) [0..]
  where
    ds = 
      [ "zero", "one", "two", "three", "four"
      , "five", "six", "seven", "eight", "nine"]
