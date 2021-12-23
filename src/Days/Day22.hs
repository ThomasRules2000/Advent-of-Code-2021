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

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = undefined

part1 :: Input -> Output1
part1 nums = undefined

part2 :: Input -> Output2
part2 nums = undefined
