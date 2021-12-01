module Days.Day23 where
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = undefined

part1 :: Input -> Output1
part1 nums = undefined

part2 :: Input -> Output2
part2 nums = undefined
