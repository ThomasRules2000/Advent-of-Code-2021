module Days.Day03 where
import           Data.List      (transpose)
import qualified Program.RunDay as R (runDay)
import           Util.Util      (binToDec)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [[Bool]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map (=='1')) . lines


part1 :: Input -> Output1
part1 xs = binToDec gr * binToDec er
    where
        gr = map mostCommon $ transpose xs
        er = map not gr


mostCommon :: [Bool] -> Bool
mostCommon bs = length (filter id bs) >= length (filter not bs)


part2 :: Input -> Output2
part2 xs = binToDec oxyRating * binToDec co2Rating
    where
        oxyRating = oxyCo2Rate True xs
        co2Rating = oxyCo2Rate False xs

oxyCo2Rate :: Bool -> [[Bool]] -> [Bool]
oxyCo2Rate _ [xs] = xs
oxyCo2Rate b xs = mc : oxyCo2Rate b (map tail $ filter ((==mc) . head) xs)
    where
        mc = (b ==) $ mostCommon $ map head xs
