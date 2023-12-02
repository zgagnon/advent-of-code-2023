#!/nix/store/biijk3a1bc18w8b9d1kjlfqbn0nyh944-cabal-install-3.10.2.1/bin/cabal
{- cabal:
build-depends: base, split, regex-compat
-}

import System.IO
import Control.Monad
import Data.Foldable
import Data.List.Split
import Data.List(isPrefixOf, isSuffixOf)
import Text.Regex (mkRegex, subRegex, Regex)
import Data.Function
import Debug.Trace


digits = ['0'..'9']

filterDigits :: String -> String
filterDigits = filter (`elem` digits)

combine :: String -> String
combine [] = []
combine (xs) = [head xs, last xs]

forwards :: (String, String) -> (String, String)
forwards (x:xs, acc) = let sub =  (acc ++ [x]) in
 if sub == "one" || x == '1' then ("1", "")
 else if sub == "two" || x == '2' then ("2", "")
 else if sub == "three" || x == '3' then ("3", "")
 else if sub == "four" || x == '4' then ("4", "")
 else if sub == "five" || x == '5' then ("5", "")
 else if sub == "six" || x == '6' then ("6", "")
 else if sub == "seven" || x == '7' then ("7", "")
 else if sub == "eight" || x == '8' then ("8", "")
 else if sub == "nine" || x == '9' then ("9", "")
 else if isPrefixOf sub "one"
  || isPrefixOf sub "two"
  || isPrefixOf sub "three"
  || isPrefixOf sub "four"
  || isPrefixOf sub "five"
  || isPrefixOf sub "six"
  || isPrefixOf sub "seven"
  || isPrefixOf sub "eight"
  || isPrefixOf sub "nine"
  then forwards (xs, sub)
  else forwards (xs, [x])

backwards :: (String, String) -> (String, String)
backwards (x:xs, acc) = let 
    sub = [x] ++ acc
  in
 if sub == "one" || x == '1' then ("1", "")
 else if sub == "two" || x == '2' then ("2", "")
 else if sub == "three" || x == '3' then  ("3", "")
 else if sub == "four" || x == '4' then ("4", "")
 else if sub == "five" || x == '5' then  ("5", "")
 else if sub == "six" || x == '6' then  ("6", "")
 else if sub == "seven" || x == '7' then  ("7", "")
 else if sub == "eight" || x == '8' then  ("8", "")
 else if sub == "nine" || x == '9' then  ("9", "")
 else if isSuffixOf sub "one"
  || isSuffixOf sub "two"
  || isSuffixOf sub "three"
  || isSuffixOf sub "four"
  || isSuffixOf sub "five"
  || isSuffixOf sub "six"
  || isSuffixOf sub "seven"
  || isSuffixOf sub "eight"
  || isSuffixOf sub "nine"
  then backwards (xs, sub)
  else backwards  (xs, [x])
 
pair :: String -> Int
pair x = let
  front = forwards (x, "")
  back = backwards (reverse x, "")
  in trace (x ++ " " ++ (fst front) ++ " " ++ (fst back)) (read (fst front ++ fst back) :: Int)

main :: IO ()
main = do
  contents <- readFile "./day-1/day-1.input"
  let split = splitOn "\n" contents
  let parsed = map pair split
  let sums = sum parsed
  print parsed
  print (length parsed)
  print sums
