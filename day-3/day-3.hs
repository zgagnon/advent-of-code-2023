#!cabal
{- cabal:
    build-depends: base, split, pretty-simple, text, containers
    default-language: Haskell2010
    executable advent-of-code2023
        main-is: Main.hs
        hs-source-dirs: app
        ghc-options: -Wall -Werror
        build-depends:
            base,
            base-compat
            

-}

import           Debug.Trace
import           Data.Char
import          Data.List
import           Data.Foldable
import          Data.Function
 

main :: IO ()
main = do
    contents <-  lines <$> readFile "day-3/day-3.input"
    let grid = zip [0..] $ map (zip [0..]) contents
    let symbols = findSymbols grid
    let partNums = findParts grid
    let actualParts = filter (isPart symbols) partNums 
    let gearRatios = do
            symbol <- symbols
            return $ findGearRatio actualParts symbol
    print $ actualParts
        >>= (\part -> [read (num part) :: Int])
        & sum
    print $ sum gearRatios
   

isPartMarker :: Char -> Bool
isPartMarker c = (not $ isDigit c) && c /= '.'

type Grid  = [(Int, [(Int, Char)])]
type Symbol = (Int, Int, Char)

isNeighbors :: PartBuilder -> (Int, Int) -> Bool
isNeighbors PartBuilder {tl=(tlRow, tlCol), br=(brRow, brCol)} (row, col) = 
    tlRow <= row && tlCol <= col && brRow >= row && brCol >= col

isPart :: [Symbol] -> PartBuilder -> Bool
isPart symbols part = let
    (tlRow, tlCol) = tl part
    (brRow, brCol) = br part
    in
        foldl (\acc (row, col,_) -> 
            acc || isNeighbors part (row, col)
        ) False symbols
    
symbolLocation :: Symbol -> Maybe (Int, Int)
symbolLocation (x, y, character) = case character of
    isDigit -> Nothing
    '.'     -> Nothing
    _       -> Just (x, y)

findSymbols  ::  Grid -> [Symbol]
findSymbols diagram = do 
        (rowIndex, row) <- diagram
        (colIndex, ch) <- row
        return (rowIndex, colIndex, ch)
    & filter (\(_,_,ch) -> isPartMarker ch) 
        
data PartBuilder = PartBuilder {tl::(Int, Int), br::(Int, Int), num::String, building::Bool} deriving Show

buildPart :: Int -> [PartBuilder] -> (Int, Char) -> [PartBuilder]
buildPart row acc (col, char) =  
    let (partBuilder, rest) = case acc of
            [] -> (PartBuilder {tl=(0,0), br=(0,0), num=[], building=False}, [])
            (x:xs) -> (x, xs)
    in 
        if isDigit char
        then case building partBuilder of
            False ->  PartBuilder {num = [char], tl=(row - 1, col - 1 ), br=(row + 1, col + 1), building=True} : acc
            True -> partBuilder {num = num partBuilder ++ [char], br=(row + 1, col + 1)} : rest
        else case building partBuilder of
            False -> acc
            True -> partBuilder {building=False} : rest

findParts :: Grid -> [PartBuilder]
findParts diagram = concat  $ map (\(rowIndex, row) ->
    reverse $ foldl (buildPart rowIndex) [] $ row ++ [(150, '.')]) diagram

findGearRatio :: [PartBuilder] -> Symbol -> Int
findGearRatio parts (row, col, ch) =
    case ch of
    '*' ->  let 
            neighbors = filter (\p -> isNeighbors p (row, col)) parts
        in
        case length neighbors of
            2 ->  let
                    first = read (num $ head neighbors) :: Int
                    second = read (num $ head $ tail neighbors):: Int
                in
                    first * second
            _ -> 0
    _   -> 0 