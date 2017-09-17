module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
type GameState = [[Card]]
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback xs ys = (a, b, c, d, e)
  where a = cCards xs ys
        b = lRanks (map cardToRank xs) (map cardToRank ys)
        c = cRanks (map cardToRank xs) (map cardToRank ys)
        d = hRanks (map cardToRank xs) (map cardToRank ys)
        e = cSuits (map cardToSuit xs) (map cardToSuit ys)

cardToRank :: Card -> Rank
cardToRank (Card suit rank) = rank

cardToSuit :: Card -> Suit
cardToSuit (Card suit rank) = suit

find :: Eq t => t -> [t] -> Int
find _ [] = 0
find x (y:ys) =
  if x == y then 1
  else find x ys

cCards :: [Card] -> [Card] -> Int
cCards [] _ = 0
cCards (x:xs) (y:ys) = find x (y:ys) + cCards xs (y:ys)
  
lRanks :: [Rank] -> [Rank] -> Int
lRanks ranks1 ranks2 = countLow ranks1 (minimum ranks2)
countLow :: [Rank] -> Rank -> Int
countLow [] _ = 0
countLow (r1:rs) rank = 
  if r1 < rank then 1 + countLow rs rank
  else countLow rs rank

cRanks :: [Rank] -> [Rank] -> Int
cRanks [] _ = 0
cRanks _ [] = 0 
cRanks (r1:r1s) (r2:r2s) = minimum [result1, result2]
  where result1 = find r1 (r2:r2s) + cRanks r1s (r2:r2s)
        result2 = find r2 (r1:r1s) + cRanks r2s (r1:r1s)

hRanks :: [Rank] -> [Rank] -> Int
hRanks ranks1 ranks2 = countHigh ranks1 (maximum ranks2)
countHigh :: [Rank] -> Rank -> Int
countHigh [] _ = 0
countHigh (r1:rs) rank =
  if r1 > rank then 1 + countHigh rs rank
  else countHigh rs rank
  
cSuits :: [Suit] -> [Suit] -> Int
cSuits [] _ = 0
cSuits _ [] = 0 
cSuits (s1:s1s) (s2:s2s) = minimum [result1, result2]
  where result1 = find s1 (s2:s2s) + cSuits s1s (s2:s2s)
        result2 = find s2 (s1:s1s) + cSuits s2s (s1:s1s)

initialGuess :: Int -> ([Card],GameState)
initialGuess n
  |n == 2 = ([(Card Club R6), (Card Heart R10)], 
    [[card1, card2] | card1 <- [minBound..maxBound]::[Card], 
      card2 <- [minBound..maxBound]::[Card], card2 /= card1])
  |n == 3 = ([(Card Club R5), (Card Diamond R8), (Card Heart Jack)],
    [[card1, card2, card3] | card1 <- [minBound..maxBound]::[Card],
      card2 <- [minBound..maxBound]::[Card], card2 /= card1,
        card3 <- [minBound..maxBound]::[Card], card3 /= card2,
          card3 /= card1])
  |n == 4 = ([(Card Club R4), (Card Diamond R7), (Card Heart R10), 
    (Card Spade King)], [[card1, card2, card3, card4] | 
      card1 <- [minBound..maxBound]::[Card], 
        card2 <- [minBound..maxBound]::[Card], card2 /= card1,
          card3 <- [minBound..maxBound]::[Card], card3 /= card2,
            card3 /= card1, card4 <- [minBound..maxBound]::[Card],
              card4 /= card3, card4 /= card2, card4 /= card1])
  |otherwise = error "not supported"
  
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (cards, c:cs) result = (newGuess, newState)
  where newState = findState (cards, c:cs) result
        newGuess = head newState
findState :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> GameState
findState (_, []) _ = []
findState (cards, c:cs) result = 
  if feedback cards c == result then c : findState (cards, cs) result
  else findState (cards, cs) result