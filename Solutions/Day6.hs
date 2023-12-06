module Solutions.Day6 (solution1, solution2) where

solution1 :: String -> Int
solution1 s = product $ do
  (t,d) <- races s
  pure $ raceSols t d

solution2 :: String -> Int
solution2 s =
  let t = read . concatMap (show . fst) $ races s
      d = read . concatMap (show . snd) $ races s
   in raceSols t d

raceSols :: Int -> Int -> Int
raceSols t d =
  let t' = fromIntegral t :: Double
      d' = fromIntegral d
      lbound = floor $ t' - sqrt (t'*t' - 4*d') :: Int
      ubound = ceiling $ t' + sqrt (t'*t' - 4*d')
   in length $ filter even [lbound + 1..ubound - 1]

races :: String -> [(Int, Int)]
races s = zip ts ds
  where
    s' = map (map read . tail . words) $ lines s
    ts = head s'
    ds = s'!!1
