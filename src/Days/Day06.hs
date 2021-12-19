module Days.Day06 where
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List.Split    (splitOn)
import qualified Program.RunDay     as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = IntMap Integer

type Output1 = Integer
type Output2 = Integer

parser :: String -> Input
parser = foldr (uncurry (IntMap.insertWith (+)) . (,1) . read) IntMap.empty . splitOn ","

part1 :: Input -> Output1
part1 = sum . (!!80) . iterate day

day :: IntMap Integer -> IntMap Integer
day = multiply . IntMap.mapKeys (subtract 1)
    where
        multiply :: IntMap Integer -> IntMap Integer
        multiply im = case IntMap.lookup (-1) im of
            Nothing -> im
            Just n -> IntMap.unionWith (+) (IntMap.fromList [(6, n), (8, n)]) $ IntMap.delete (-1) im

part2 :: Input -> Output2
part2 = sum . (!!256) . iterate day
