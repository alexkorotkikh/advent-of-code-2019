module Day3 where

import           Data.List.Split
import           Data.List
import           Data.Either
import qualified Data.Set                      as S


data Direction = Up Int
                | Down Int
                | Leftt Int
                | Rightt Int

-- data Point = Point { x :: Int
--                     , y :: Int
--                     } deriving Show

-- data WireSegment = WireSegment { start :: Point
--                                , end :: Point
--                                } deriving Show

type Point = (Int, Int)
type Wire = [Point]

mapDirection :: String -> Direction
mapDirection ('U' : rest) = Up (read rest :: Int)
mapDirection ('D' : rest) = Down (read rest :: Int)
mapDirection ('L' : rest) = Leftt (read rest :: Int)
mapDirection ('R' : rest) = Rightt (read rest :: Int)

parseWire :: String -> Wire
parseWire s =
    -- change to foldl and reverse
  let directions = map mapDirection (splitOn "," s)
      segments   = foldl
        (\l d ->
          let (x, y) = if null l then (0, 0) else last l
          in  l ++ (case d of
                Up     n -> [ (x, y') | y' <- [y .. (y + n)] ]
                Down   n -> reverse $ [ (x, y') | y' <- [(y - n) .. y] ]
                Rightt n -> [ (x', y) | x' <- [x .. (x + n)] ]
                Leftt  n -> reverse $ [ (x', y) | x' <- [(x - n) .. x] ]
              )
                
        )
        []
        directions
  in  segments


findCrosses :: Wire -> Wire -> [Point]
findCrosses as bs = filter (/= (0, 0))as `intersect` bs

distance :: Point -> Point -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

main :: IO ()
main = do
  content <- readFile "day-3-input.txt"
  let wires   = map parseWire (lines content)
  let crosses = findCrosses (head wires) (wires !! 1)
  print crosses
  let closestCross = minimum $ map (distance (0, 0)) crosses
  print closestCross
