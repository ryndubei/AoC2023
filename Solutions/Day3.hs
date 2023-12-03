module Solutions.Day3 (solution1, solution2) where

import Data.List (groupBy)
import Data.Char (isDigit, isSpace)
import Control.Monad (guard)
import Data.List.Extra (linesBy, nubOrd)
import Control.Monad.State (execStateT, lift, modify)
import qualified Data.Map.Strict as Map

solution1 :: String -> Int
solution1 s = sum $ do
  l <- adjacents' $ linesPadded s
  num <- groupBy (\(a,_) (b,_) -> isDigit a && isDigit b) l
  guard $ all (isDigit . fst) num
  guard $ any (any isSymbol' . snd) num
  pure $ read (map fst num)

solution2 :: String -> Int
solution2 s = sum $ Map.elems gearRatios
  where
    s' = zip [0..] . unlines $ linesPadded s
    gears = filter ((=='*') . snd) s'
    gearRatios = Map.map product
      . Map.filter ((==2) . length) 
      . Map.unionsWith (++) $ do
        l <- adjacents' $ linesBy (isSpace . snd) s'
        let gearRatios' = Map.unionsWith (++) . (`execStateT` mempty) $ do
              num <- lift $ groupBy (\((_,a),_) ((_,b),_) -> isDigit a && isDigit b) l
              guard $ all (isDigit . snd . fst) num
              guard $ any (any (isSymbol' . snd) . snd) num
              let numGears = nubOrd [fst n' | (_, n) <- num, n' <- n, n' `elem` gears]
              let i = read $ map (snd . fst) num :: Int
              mapM_ (\g -> modify (Map.insert g [i])) numGears
        pure gearRatios'

linesPadded :: String -> [String]
linesPadded s =
  let ls = lines s
      n = length $ head ls
      pad = replicate n '.'
   in [pad] ++ map (\l -> "." ++ l ++ ".") ls ++ [pad]

isSymbol' :: Char -> Bool
isSymbol' '.' = False
isSymbol' c = not (isDigit c || isSpace c)

adjacents' :: [[a]] -> [[(a, [a])]]
adjacents' ls = do
  (prev, l, next) <- adjacents ls
  let l' = adjacents l
      prev' = adjacents prev
      next' = adjacents next
  pure $ do
    ((a,b,c),(d,e,f),(g,h,i)) <- zip3 l' prev' next'
    pure (b, [a,c,d,e,f,g,h,i])

adjacents :: [a] -> [(a,a,a)]
adjacents prevs =
  let xs = tail prevs
      nexts = tail xs
   in zip3 prevs xs nexts
