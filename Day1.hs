module Day1 where

fuelForMass :: Int -> Int
fuelForMass m | m' <= 0 = 0
              | m' > 0  = m' + fuelForMass m'
  where m' = m `div` 3 - 2

main :: IO ()
main = do
  content <- readFile "day-1-input.txt"
  let masses    = map (\x -> read x :: Int) (lines content)
  let perModule = map fuelForMass masses
  print $ sum perModule
