module StringProcessing 
( trim
, stringToInt
, innerPadJoin
)
where

import Data.Char

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

stringToInt :: String -> Int
stringToInt s = read s :: Int

innerPadJoin :: Int -> String -> String -> String
innerPadJoin n start end = start ++ (replicate (n - length start - length end) ' ') ++ end
   
innerPadJoin :: Int -> String -> String -> String
innerPadJoin n start end = start ++ replicate (n - length start - length end) ' ' ++ end
