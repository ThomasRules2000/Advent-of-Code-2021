module Days.Day01 where
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map read . lines

part1 :: Input -> Output1
part1 = numIncrease 1

numIncrease :: Int -> [Int] -> Int
numIncrease winSize xs = length $ filter id $ zipWith (<) xs $ drop winSize xs

part2 :: Input -> Output2
part2 = numIncrease 3