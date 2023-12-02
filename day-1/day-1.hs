#!/nix/store/biijk3a1bc18w8b9d1kjlfqbn0nyh944-cabal-install-3.10.2.1/bin/cabal
{- cabal:
build-depends: base, split, regex-compat
-}

import System.IO
import Data.Foldable
import Data.List.Split
import Data.List(isPrefixOf, isSuffixOf)
import Data.Function
import Debug.Trace

parse :: (String -> Char -> String) -> ([Char] -> [Char] -> Bool) -> (String, String) -> (String, String)
parse subStringer compare (x:xs, acc) = let sub = trace ([x] ++ " " ++ xs ++ " " ++ acc) (subStringer acc x) in
 if sub == "one" || x == '1' then ("1", "")
 else if sub == "two" || x == '2' then ("2", "")
 else if sub == "three" || x == '3' then ("3", "")
 else if sub == "four" || x == '4' then ("4", "")
 else if sub == "five" || x == '5' then ("5", "")
 else if sub == "six" || x == '6' then ("6", "")
 else if sub == "seven" || x == '7' then ("7", "")
 else if sub == "eight" || x == '8' then ("8", "")
 else if sub == "nine" || x == '9' then ("9", "")
 else if compare sub "one"
  || compare sub "two"
  || compare sub "three"
  || compare sub "four"
  || compare sub "five"
  || compare sub "six"
  || compare sub "seven"
  || compare sub "eight"
  || compare sub "nine"
  then parse subStringer compare (xs, sub)
  else parse subStringer compare (xs, [x])

forwards :: (String, String) -> (String, String)
forwards (x:xs, acc) = parse (\acc x -> acc ++ [x]) isPrefixOf (x:xs, acc)

backwards :: (String, String) -> (String, String)
backwards (x:xs, acc) = parse (\acc x -> [x] ++ acc) isSuffixOf (x:xs, acc)
 
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
