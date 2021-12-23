module Days.Day19 where
import           Data.Bifunctor  (bimap, second)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (listToMaybe, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import           System.Clock    (TimeSpec)
import qualified Util.Set        as Set
import           Util.Util       (listToTuple3)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int, Int)
type Constellation = Set (Pos, Set Pos)

type Input = Map Constellation Pos

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = getMapping . map (Set.fromList . map (listToTuple3 . map read . splitOn ",") . tail . lines) . splitOn "\n\n"

part1 :: Input -> Output1
part1 mapping = Set.size $ Set.unions $ map (\(c,p) -> Set.map (addPoint p . fst) c) $ Map.toList mapping

getMapping :: [Set Pos] -> Map Constellation Pos
getMapping scanners = search Set.empty s0 (Map.singleton s0 (0,0,0)) rest
    where (s0:rest) = map constellation scanners

search :: Set Constellation -> Constellation -> Map Constellation Pos -> [Constellation] -> Map Constellation Pos
search _ _ found [] = found
search tried curr found toFind = search newTried next newFound newToFind
    where
        newFound = Map.union found $ Map.fromList $ map (second (addPoint $ found Map.! curr)) $ concatMap (mapMaybe (getIntersection curr) . rotatedCons) toFind
        newToFind = filter (all (`Map.notMember` newFound) . rotatedCons) toFind
        newTried = Set.insert curr tried
        next = Set.findMin $ Map.keysSet newFound Set.\\ newTried

rotations :: [Pos -> Pos]
rotations = [\(x, y, z) -> (-z, -y, -x), \(x, y, z) -> (-z, -x,  y), \(x, y, z) -> (-z, x, -y), \(x, y, z) -> (-z, y,  x),
             \(x, y, z) -> (-y, -z,  x), \(x, y, z) -> (-y, -x, -z), \(x, y, z) -> (-y, x,  z), \(x, y, z) -> (-y, z, -x),
             \(x, y, z) -> (-x, -z, -y), \(x, y, z) -> (-x, -y,  z), \(x, y, z) -> (-x, y, -z), \(x, y, z) -> (-x, z,  y),
             \(x, y, z) -> ( x, -z,  y), \(x, y, z) -> ( x, -y, -z), \(x, y, z) -> ( x, y,  z), \(x, y, z) -> ( x, z, -y),
             \(x, y, z) -> ( y, -z, -x), \(x, y, z) -> ( y, -x,  z), \(x, y, z) -> ( y, x, -z), \(x, y, z) -> ( y, z,  x),
             \(x, y, z) -> ( z, -y,  x), \(x, y, z) -> ( z, -x, -y), \(x, y, z) -> ( z, x,  y), \(x, y, z) -> ( z, y, -x)]

constellation :: Set Pos -> Constellation
constellation s = Set.map (\p1 -> (p1, Set.filter (/=(0,0,0)) $ Set.map (diffPoint p1) s)) s

rotatedCons :: Constellation -> [Constellation]
rotatedCons cs = map (\rot -> Set.map (bimap rot (Set.map rot)) cs) rotations

getIntersection :: Constellation -> Constellation -> Maybe (Constellation, Pos)
getIntersection xs ys = (ys,) <$> listToMaybe [diffPoint p2 p1 | (p1, s1) <- Set.toList xs, (p2, s2) <- Set.toList ys, Set.size (s1 Set./\ s2) >= 11]

diffPoint :: Pos -> Pos -> Pos
diffPoint (x1, y1, z1) (x2, y2, z2) = (x2-x1, y2-y1, z2-z1)

addPoint :: Pos -> Pos -> Pos
addPoint (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

part2 :: Input -> Output2
part2 mapping = maximum [manhattanDist p1 p2 | p1 <- points, p2 <- points]
    where points = Map.elems mapping

manhattanDist :: Pos -> Pos -> Int
manhattanDist (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
