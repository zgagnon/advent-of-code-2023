module Day7 where

import           Data.Char       (isDigit)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Map        (elems, empty, insertWith)
import           Data.Ord        (Down (..), comparing)
import           Debug.Trace

data HandType = High | One | Two | Three | Full | Four | Five deriving (Show, Eq, Ord)

data Hand = Hand {
    handType :: HandType
    ,hand    :: String
    ,bid     :: Int
} deriving (Show, Eq, Ord)

playDay7Part2 :: [String] -> Int
playDay7Part2 input = undefined

playDay7Part1 :: [String] -> Int
playDay7Part1 input = let
    hands = parseHands input
    rankedHands = rankHands hands
    in scoreRankedHands rankedHands

parseHands :: [String] -> [Hand]
parseHands hands = do
    hand <- hands
    let parts = splitOn " " hand
    let handType = matchHandType $ head parts
    return $ Hand {
        hand = head parts
        , bid = stringToInt $  parts !! 1
        , handType = handType
    }

matchHandType :: String -> HandType
matchHandType hand = let
    count = foldl (\acc x -> insertWith (+) x 1 acc) empty hand
    cardCounts :: [Int] = sortBy (comparing Data.Ord.Down) (elems count)
    in case  cardCounts of
        5:_   -> Five
        4:_   -> Four
        3:2:_ -> Full
        3:_   -> Three
        2:2:_ -> Two
        2:_   -> One
        _     -> High


stringToInt :: String -> Int
stringToInt s = read s :: Int

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 = let
    handType1 = handType hand1
    handType2 = handType hand2
    in case compare handType1 handType2 of
        EQ -> compareHandValues (hand hand1) (hand hand2)
        x  -> x

compareHandValues :: String -> String -> Ordering
compareHandValues (x:xs) (y:ys)
    | x == y = compareHandValues xs ys
    | otherwise = compareFaceCards x y

compareFaceCards :: Char -> Char -> Ordering
compareFaceCards x y
    | isDigit x && isDigit y = compare x y
    | isDigit x = LT
    | isDigit y = GT
    | x == y = EQ
    | x == 'A' = GT
    | y == 'A' = LT
    | x == 'K' = GT
    | y == 'K' = LT
    | x == 'Q' = GT
    | y == 'Q' = LT
    | x == 'J' = GT
    | otherwise = LT

rankHands :: [Hand] -> [(Int, Hand)]
rankHands hands =  zip [1..] $ sortBy compareHands hands

scoreRankedHands :: [(Int, Hand)] -> Int
scoreRankedHands rankedHands = sum $ map (\(rank, hand) -> rank * bid hand) rankedHands
