module Days.Day08 where
import qualified Program.RunDay as R (runDay)
import Data.List.Split
import Util.Util (listToTuple)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [([String], [String])]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (listToTuple . map words . splitOn " | ") . lines

part1 :: Input -> Output1
part1 = length . filter (`elem` [2,3,4,7]) . concatMap (map length . snd)

part2 :: Input -> Output2
part2 = undefined
