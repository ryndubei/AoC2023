module Solutions.Day3 (solution1, solution2) where

import Data.List (groupBy)
import Data.Char (isDigit, isSpace)
import Control.Monad (guard, mzero)
import Text.Read (readMaybe)
import Data.List.Extra (linesBy, nubOrd)
import qualified Data.Map.Strict as Map

solution1 :: String -> Int
solution1 s = sum $ do
  l <- adjacents $ linesPadded s
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
    s' = zip [0..] . unlines $ linesPadded s
    gears = filter ((=='*') . snd) s'

linesPadded :: String -> [String]
linesPadded s = ([pad] ++) . (++ [pad]) . map ((++ ".") . ("." ++)) $ ls
  where
    ls = lines s
    pad = replicate (length $ head ls) '.'

adjacents :: [[a]] -> [[(a, [a])]]
adjacents ls = do
  (prev, l, next) <- adj ls
  pure $ do
    ((a,b,c),(d,e,f),(g,h,i)) <- zip3 (adj l) (adj prev) (adj next)
    pure (b, [a,c,d,e,f,g,h,i])
  where
    adj xs = zip3 xs (tail xs) (tail $ tail xs)
