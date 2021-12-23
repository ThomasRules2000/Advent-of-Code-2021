module Days.Day05 where
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Program.RunDay  as R (runDay)
import           System.Clock    (TimeSpec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type Input = [Line]

type Output1 = Int
type Output2 = Int

type Point = (Int, Int)

type Line = (Point, Point)

parser :: String -> Input
parser = map (listToTuple . map (listToTuple . map read . splitOn ",") . splitOn " -> ") . lines

part1 :: Input -> Output1
part1 = overlaps
      . filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

overlaps :: [Line] -> Int
overlaps = Map.size
         . Map.filter (>1)
         . Map.unionsWith (+)
         . map (Map.fromList . map (,1) . points)

-- overlaps :: [Line] -> Int
-- overlaps = Map.size
--          . Map.filter (>1)
--          . foldr (uncurry (Map.insertWith (+)) . (,1)) Map.empty
--          . concatMap points

points :: Line -> [Point]
points (p1@(x1, y1), p2@(x2, y2))
    | p1 == p2  = [p1]
    | otherwise = zip xs ys
    where
        xs = [x1, x1 + signum (x2 - x1) .. x2]
        ys = [y1, y1 + signum (y2 - y1) .. y2]

part2 :: Input -> Output2
part2 = overlaps
