module Day3 where

import           Data.List.Split

data Direction = Up Int
                | Down Int
                | Leftt Int
                | Rightt Int

data Point = Point { x :: Int
                    , y :: Int
                    } deriving Show

data WireSegment = WireSegment { start :: Point
                               , end :: Point
                               } deriving Show

mapDirection :: String -> Direction
mapDirection ('U' : rest) = Up (read rest :: Int)
mapDirection ('D' : rest) = Down (read rest :: Int)
mapDirection ('L' : rest) = Leftt (read rest :: Int)
mapDirection ('R' : rest) = Rightt (read rest :: Int)

parseWire :: String -> [WireSegment]
parseWire s =
    -- change to foldl and reverse
  let directions = map mapDirection (splitOn "," s)
      segments   = foldl
        (\l d ->
          let point = if null l then Point 0 0 else end (last l)
          in  WireSegment
                  { start = point
                  , end   = case d of
                              Up     n -> Point { x = x point, y = y point + n }
                              Down   n -> Point { x = x point, y = y point - n }
                              Leftt  n -> Point { x = x point - n, y = y point }
                              Rightt n -> Point { x = x point + n, y = y point }
                  }
                : l
        )
        []
        directions
    -- Wire [WireSegment (Point 0 0) (Point 0 0)]
  in  reverse segments


findCrosses :: [WireSegment] -> [WireSegment] -> [Point]
findCrosses as bs = 
  let options = [ (a, b) | a <- as, b <- bs ]
      eitherIntersections = map (\ab pq ->
        case determinant of
          0 -> Left "(Parallel lines – no intersection)"
          _ ->
            let delta f x = f (fst x) - f (snd x)
                diff [a, b, c, d] = a * d - b * c
                [abDX, pqDX, abDY, pqDY] = [delta fst, delta snd] <*> [ab, pq]
                determinant = diff [abDX, abDY, pqDX, pqDY]
                [abD, pqD] = (\(a, b) -> diff ([fst, snd] <*> [a, b])) <$> [ab, pq]
                [ix, iy] =
                  [\(ab, pq) -> diff [abD, ab, pqD, pq] / determinant] <*>
                  [(abDX, pqDX), (abDY, pqDY)]
            in Right (ix, iy)
          ) options
    in filter isRight eitherIntersections
  

distance :: Point -> Point -> Int
distance _ _ = 0

main :: IO ()
main = do
  content <- readFile "day-3-input.txt"
  let wires = map parseWire (lines content)
  let crosses      = findCrosses (head wires) (wires !! 1)
  print crosses
  let closestCross = minimum $ map (distance (Point 0 0)) crosses
  print closestCross
