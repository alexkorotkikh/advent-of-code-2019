module Day2 where

import           Data.List

changeNthElement :: Int -> a -> [a] -> [a]
changeNthElement idx newElement list
  | idx < 0 = list
  | otherwise = case splitAt idx list of
    (front, element : back) -> front ++ newElement : back
    _                       -> list    -- if the list doesn't have an element at index idx

intcodeComputer :: Int -> [Int] -> [Int]
intcodeComputer index program = case drop index program of
  (99                        : rest) -> program
  (op : idx1 : idx2 : target : rest) -> intcodeComputer
    (index + 4)
    (changeNthElement target res program)
   where
    arg1 = program !! idx1
    arg2 = program !! idx2
    res  = case op of
      1 -> arg1 + arg2
      2 -> arg1 * arg2


splitBy delimiter = foldr f [[]]
 where
  f c l@(x : xs) | c == delimiter = [] : l
                 | otherwise      = (c : x) : xs

findInput :: [Int] -> Int -> Maybe (Int, Int)
findInput program target =
  let options = [ (a, b) | a <- [0 .. 99], b <- [0 .. 99] ]
  in
    find
      (\t ->
        let program' =
                changeNthElement 1 (fst t) $ changeNthElement 2 (snd t) program
            result = intcodeComputer 0 program'
        in  head result == target
      )
      options


main :: IO ()
main = do
  content <- readFile "day-2-input.txt"
  let program    = map (\x -> read x :: Int) (splitBy ',' content)
  let newProgram = intcodeComputer 0 program
  print (head newProgram)

  let requiredInput = findInput program 19690720
  print requiredInput
