module Days.Day07 where
import           Data.List.Split (splitOn)
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map read . splitOn ","

part1 :: Input -> Output1
part1 subs = minimum $ map (`fuelCost` subs) [minimum subs .. maximum subs]
    where
        fuelCost :: Int -> [Int] -> Int
        fuelCost pos = sum . map (abs . subtract pos)

part2 :: Input -> Output2
part2 subs = minimum $ map (`fuelCost` subs) [minimum subs .. maximum subs]
    where
        fuelCost :: Int -> [Int] -> Int
        fuelCost pos = sum . map ((\x -> x * (x+1) `div` 2) . abs . subtract pos)
