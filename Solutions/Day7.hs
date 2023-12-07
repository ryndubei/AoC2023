{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Solutions.Day7 (solution1, solution2) where

import Data.Char (isDigit)
import Data.List (elemIndex, nub, sortOn, sort)
import Data.Maybe (fromJust)
import Data.Ord (Down(Down))

solution :: (Char -> Card) -> String -> Int
solution f s = sum . zipWith (*) [1..] . map snd . sort $ do
  [cards, bid] <- map words $ lines s
  let [a,b,c,d,e] = map f cards
      deck = Deck (a,b,c,d,e)
  pure (deck, read bid)

solution1 :: String -> Int
solution1 = solution readCard

solution2 :: String -> Int
solution2 = solution readCard'

newtype Card = Card Int
  deriving (Eq, Ord)

newtype Deck = Deck (Card, Card, Card, Card, Card) deriving Eq

instance Ord Deck where
  compare (Deck d1) (Deck d2)
    | t1 /= t2 = compare t1 t2
    | otherwise = compare d1 d2
      where
        cards (a,b,c,d,e) = [a,b,c,d,e]
        t1 = deckType' (cards d1)
        t2 = deckType' (cards d2)

readCard :: Char -> Card
readCard c
  | isDigit c && c `notElem` "01" = Card $ read [c]
  | otherwise = Card . (+ 10) . fromJust $ elemIndex c "TJQKA"

readCard' :: Char -> Card
readCard' 'J' = Card 1
readCard' c = readCard c

deckType :: [Card] -> [Int]
deckType cards = sortOn Down
  . map (\card -> length $ filter (== card) cards) $
  nub cards

deckType' :: [Card] -> [Int]
deckType' cards =
  let t' = deckType (filter (/= Card 1) cards)
      jokers = length $ filter (== Card 1) cards
   in case t' of
    [] -> deckType cards
    (x:xs) -> x + jokers : xs
