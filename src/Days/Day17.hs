module Days.Day17 where
import           Data.List.Split (splitOn)
import qualified Program.RunDay  as R (runDay)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe Integer, Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = ((Int, Int), (Int, Int))

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = listToTuple . map (listToTuple . map read . splitOn "..") . splitOn ", y=" . drop 15 . head . lines

part1 :: Input -> Output1
part1 = maximum . concat . validYs . snd

moveY :: Int -> Int -> Int -> [Int]
moveY yMin n v
    | n < yMin  = []
    | otherwise = n : moveY yMin (n+v) (v-1)

validYs :: (Int, Int) -> [[Int]]
validYs (yMin, yMax) = filter (any (<= yMax)) $ map (moveY yMin 0) [-maxVel..maxVel]
    where maxVel = abs yMin + 1

part2 :: Input -> Output2
part2 box@(x,y) = length $ filter (any $ validPos box) $ [zip xs ys | xs <- validXs x, ys <- validYs y]

moveX :: Int -> Int -> [Int]
moveX n v
    | v == 0    = repeat n
    | otherwise = n : moveX (n+v) (v-1)

validXs :: (Int, Int) -> [[Int]]
validXs (xMin, xMax) = map (moveX 0) [minVel..xMax+1]
    where minVel = floor $ sqrt $ 2 * fromIntegral xMin

validPos :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
validPos ((xMin, xMax), (yMin, yMax)) (x,y) = and [xMin <= x, x <= xMax, yMin <= y, y <= yMax]
