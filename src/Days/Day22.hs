module Days.Day22 where
import           Data.Bifunctor   (bimap, first)
import           Data.Foldable    (foldl')
import           Data.List.Split  (splitOn)
import           Data.Tuple.Extra (swap, uncurry3)
import qualified Program.RunDay   as R (runDay)
import           System.Clock     (TimeSpec)
import           Util.Util        (liftTup, listToTuple, listToTuple3)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type Point = (Int, Int, Int)

type Input = [(Cuboid, Bool)]

type Output1 = Int
type Output2 = Int

data Cuboid = Cuboid (Int, Int) (Int, Int) (Int, Int)
    deriving (Eq, Ord, Show)

parser :: String -> Input
parser = map parseCuboid . lines

parseCuboid :: String -> (Cuboid, Bool)
parseCuboid = swap
            . bimap (=="on") (uncurry3 Cuboid . listToTuple3 . map (listToTuple . map read . splitOn ".." . drop 2) . splitOn ",")
            . listToTuple . words

part1 :: Input -> Output1
part1 = getOn . filter (insideRegion . fst)

insideRegion :: Cuboid -> Bool
insideRegion (Cuboid (xMin, xMax) (yMin, yMax) (zMin, zMax)) = all (<=50) [xMin, yMin, zMin] && all (>= (-50)) [xMax, yMax, zMax]

getOn :: [(Cuboid, Bool)] -> Int
getOn = sum . map (area . fst) . filter snd . foldl' processCuboid []

processCuboid :: [(Cuboid, Bool)] -> (Cuboid, Bool) -> [(Cuboid, Bool)]
processCuboid s c = (c:) $ concatMap (liftTup . first (`intersectCuboids` fst c)) s

area :: Cuboid -> Int
area (Cuboid (xMin, xMax) (yMin, yMax) (zMin, zMax)) = (xMax-xMin+1) * (yMax-yMin+1) * (zMax-zMin+1)

intersectCuboids :: Cuboid -> Cuboid -> [Cuboid]
intersectCuboids c0@(Cuboid x0 y0 z0) c1@(Cuboid x1 y1 z1)
    | any null [xi, yi, zi] = [c0]
    | otherwise = [Cuboid x y z | x <- xi<>getUnchanged x0 x1, y <- yi<>getUnchanged y0 y1, z <- zi<>getUnchanged z0 z1, x/=head xi || y/=head yi || z/=head zi]
    where
        xi = getIntersection x0 x1
        yi = getIntersection y0 y1
        zi = getIntersection z0 z1

getUnchanged :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getUnchanged (min0, max0) (min1, max1)
    | max0 < min1 || max1 < min0 = [(min0, max0)] -- No intersection
    | min0 >= min1 && max0 <= max1 = [] -- 0 is completely inside 1 (all change)
    | min0 <  min1 && max0 >  max1 = [(min0, min1-1), (max1+1, max0)] -- 1 is completely inside 0 (middle change, left and right unchanged)
    | min0 < min1 = [(min0, min1-1)] -- Change is on the right
    | otherwise = [(max1+1, max0)] -- Change is on the left

getIntersection :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getIntersection (min0, max0) (min1, max1)
    | max0 < min1 || max1 < min0 = []
    | min0 >= min1 && max0 <= max1 = [(min0, max0)]
    | min0 <  min1 && max0 >  max1 = [(min1, max1)]
    | min0 < min1 = [(min1, max0)]
    | otherwise = [(min0, max1)]

part2 :: Input -> Output2
part2 = getOn
