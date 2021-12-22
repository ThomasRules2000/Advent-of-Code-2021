module Days.Day03 where
import           Data.List        (transpose)
import           Data.Tuple.Extra (both)
import qualified Program.RunDay   as R (runDay)
import           Util.Util        (binToDec)

runDay :: String -> IO (Maybe Integer, Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [[Bool]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map (=='1')) . lines


part1 :: Input -> Output1
part1 xs = binToDec gamma * binToDec epsilon
    where
        gamma   = mostCommon <$> transpose xs
        epsilon = not <$> gamma

mostCommon :: [Bool] -> Bool
mostCommon bs = length (filter id bs) >= length (filter not bs)


part2 :: Input -> Output2
part2 xs = uncurry (*) $ both (binToDec . o2CO2Rate xs) (True, False)

o2CO2Rate :: [[Bool]] -> Bool -> [Bool]
o2CO2Rate [x] _ = x
o2CO2Rate xs  b = mc : o2CO2Rate (tail <$> filter ((==mc) . head) xs) b
    where mc = (b ==) $ mostCommon $ map head xs
