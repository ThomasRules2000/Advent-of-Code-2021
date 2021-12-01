module Days.Day01 where
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = fmap read . lines

part1 :: Input -> Output1
part1 = numIncrease

-- Explicit Recursion
-- numIncrease :: [Int] -> Int
-- numIncrease [] = 0
-- numIncrease [x] = 0
-- numIncrease (x:y:xs)
--     | x < y = 1 + rest
--     | otherwise = rest
--     where rest = numIncrease (y:xs)

numIncrease :: [Int] -> Int
numIncrease xs = length $ filter id $ zipWith (<) xs $ tail xs

part2 :: Input -> Output2
part2 nums = numIncrease $ zipWith (+) nums $ zipWith (+) (tail nums) $ drop 2 nums
