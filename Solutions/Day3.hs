module Solutions.Day3 (solution1, solution2) where

import Data.List (groupBy, transpose)
import Data.Char (isDigit, isSpace)
import Control.Monad (guard, mzero)
import Text.Read (readMaybe)
import Data.List.Extra (linesBy, nubOrd, dropEnd)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

solution1 :: String -> Int
solution1 s = sum $ do
  l <- adjacents $ lines s
  num <- groupBy (\(a,_) (b,_) -> isDigit a && isDigit b) l
  i <- maybe mzero pure (readMaybe (map fst num))
  guard $ any (any isSymbol' . snd) num
  pure i
  where
    isSymbol' '.' = False
    isSymbol' c = not (isDigit c || isSpace c)

solution2 :: String -> Int
solution2 s = sum 
  . Map.map product 
  . Map.filter ((==2) . length)
  . Map.unionsWith (++) $ do
      l <- adjacents $ linesBy (isSpace . snd) s'
      num <- groupBy (\((_,a),_) ((_,b),_) -> isDigit a && isDigit b) l
      i <- maybe mzero pure (readMaybe (map (snd . fst) num))
      let numGears = nubOrd [fst n' | (_, n) <- num, n' <- n, n' `elem` gears]
      pure . Map.fromList $ map (,[i]) numGears
  where
    s' = zip [0..] s
    gears = filter ((=='*') . snd) s'

adjacents :: [[a]] -> [[(a, [a])]]
adjacents ls = do
  (l, adjs) <- adj ls
  let adjs' = map adj adjs
  pure $ do
    ((c, r1), r2) <- zip (adj l) (transpose adjs')
    pure (c, r1 ++ map fst r2 ++ concatMap snd r2)
  where
    adj xs = 
      let xs' = map Just xs
       in zipWith3 (\a b c -> (a, catMaybes [b,c])) xs (Nothing : dropEnd 1 xs') (drop 1 xs' ++ [Nothing])
