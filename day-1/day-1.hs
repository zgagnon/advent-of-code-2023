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
parse subStringer compare (x:xs, acc) = let sub =  (subStringer acc x) in
  case (reads [x]) :: [(Int, String)] of
      [( _, "")] -> ([x], "")
      _         -> if sub == "one" || x == '1' then ("1", "")
      else if sub == "two" then ("2", "")
      else if sub == "three" then ("3", "")
      else if sub == "four"  then ("4", "")
      else if sub == "five" then ("5", "")
      else if sub == "six"  then ("6", "")
      else if sub == "seven"  then ("7", "")
      else if sub == "eight"  then ("8", "")
      else if sub == "nine"  then ("9", "")
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

-- Build from the front, so we add new characters to the end of the substring
-- and check if the substring is a prefix of the target string
forwards :: (String, String) -> (String, String)
forwards (x:xs, acc) = parse (\acc x -> acc ++ [x]) isPrefixOf (x:xs, acc)

-- Build from the back, so we add new characters to the front of the substring
-- and check if the substring is a suffix of the target string
backwards :: (String, String) -> (String, String)
backwards (x:xs, acc) = parse (\acc x -> [x] ++ acc) isSuffixOf (x:xs, acc)
 
pair :: String -> Int
pair x = let
  front = forwards (x, "")
  back = backwards (reverse x, "")
  in trace (x ++ " " ++ (fst front) ++ " " ++ (fst back)) (read (fst front ++ fst back) :: Int)

main :: IO ()
main = do
  contents <- readFile "./day-1/day-1.input.txt"
  let split = splitOn "\n" contents
  let parsed = map pair split
  let sums = sum parsed
  print parsed
  print (length parsed)
  print sums
