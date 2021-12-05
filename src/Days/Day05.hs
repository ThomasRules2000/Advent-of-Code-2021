module Days.Day05 where
import           Control.DeepSeq (NFData)
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           GHC.Generics    (Generic)
import qualified Program.RunDay  as R (runDay)
import           Util.Util

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [Line]

type Output1 = Int
type Output2 = Int

type Point = (Int, Int)

data Line = Line Point Point
    deriving (Eq, Ord, Show, Generic, NFData)

parser :: String -> Input
parser = map (uncurry Line . listToTuple . map (listToTuple . map read . splitOn ",") . splitOn " -> ") . lines

part1 :: Input -> Output1
part1 = overlaps
      . filter (\(Line (x1, y1) (x2, y2)) -> x1 == x2 || y1 == y2)

overlaps :: [Line] -> Int
overlaps = Map.size
         . Map.filter (>1) 
         . Map.unionsWith (+) 
         . map (Map.fromSet (const 1) . points) 

points :: Line -> Set Point
points (Line p1@(x1, y1) p2@(x2, y2))
    | p1 == p2 = Set.singleton p1
    | otherwise = Set.fromList $ zip xs ys
    where
        xs = [x1, x1 + signum (x2 - x1) .. x2]
        ys = [y1, y1 + signum (y2 - y1) .. y2]

part2 :: Input -> Output2
part2 = overlaps
