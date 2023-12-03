module Solutions.Day2 where

import Control.Monad (guard)
import Data.List.Extra (wordsBy)
import Data.List (foldl')
import Data.Bifunctor (bimap)

solution1 :: String -> Int
solution1 s = sum $ do
  l <- lines s
  let (gid, l') = unGameId l
  guard $ all (\(r,g,b) -> (r <= 12) && (g <= 13) && (b <= 14)) (selections l')
  pure gid

solution2 :: String -> Int
solution2 s = sum $ do
  l <- lines s
  let sels = selections . snd $ unGameId l
  pure $
    maximum (map (\(a,_,_) -> a) sels)
    * maximum (map (\(_,b,_) -> b) sels)
    * maximum (map (\(_,_,c) -> c) sels)

unGameId :: String -> (Int, String)
unGameId = bimap (read . drop 5) (drop 1) . break (== ':')

selections :: String -> [(Int, Int, Int)]
selections l =
  let items = map (map (drop 1) . wordsBy (== ',')) $ wordsBy (== ';') l
   in map countSelection items

countSelection :: [String] -> (Int, Int, Int)
countSelection = (`foldl'` (0,0,0)) $ \(r,g,b) item ->
    let (i, col) = break (== ' ') item
        i' = read i :: Int
    in case col of
      " red" -> (r + i',g,b)
      " green" -> (r,g + i',b)
      " blue" -> (r,g,b + i')
      _ -> error "no"
