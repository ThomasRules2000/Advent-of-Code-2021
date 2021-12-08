module Days.Day08 where
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Program.RunDay as R (runDay)
import           Util.Util

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = ([([Set Char], [Set Char])])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (listToTuple . map (map Set.fromList . words) . splitOn " | ") . lines

part1 :: Input -> Output1
part1 = length . filter (`elem` [2,3,4,7]) . concatMap (map Set.size . snd)

part2 :: Input -> Output2
part2 = sum . map processLine

processLine :: ([Set Char], [Set Char]) -> Int
processLine (nums, out) = readNum (getNumMappings nums) out

readNum :: Map (Set Char) Int -> [Set Char] -> Int
readNum m = foldl (\acc s -> 10 * acc + m Map.! s) 0

getNumMappings :: [Set Char] -> Map (Set Char) Int
getNumMappings ss = Map.fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
    where
        getOneOfLength :: Int -> [Set Char] -> Set Char
        getOneOfLength n = head . filter ((==n) . Set.size)

        one   = getOneOfLength 2 ss
        four  = getOneOfLength 4 ss
        seven = getOneOfLength 3 ss
        eight = getOneOfLength 7 ss
        three = head $ filter (\s -> (Set.size s == 5) && seven `Set.isSubsetOf` s) ss
        nine  = head $ filter (\s -> (Set.size s == 6) && four `Set.isSubsetOf` s) ss
        zero  = head $ filter (\s -> (Set.size s == 6) && seven `Set.isSubsetOf` s && not (s == nine)) ss
        six   = head $ filter (\s -> (Set.size s == 6) && not (s `elem` [nine, zero])) ss
        five  = head $ filter (\s -> (Set.size s == 5) && s `Set.isSubsetOf` six) ss
        two   = head $ filter (\s -> (Set.size s == 5) && not (s `elem` [three, five])) ss