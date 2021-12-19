module Days.Day04 where
import           Data.List       (transpose)
import           Data.List.Split (splitOn)
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = ([Int], [Board])

type Output1 = Int
type Output2 = Int

type Board = [[(Int, Bool)]]

parser :: String -> Input
parser inp = (parseNums nums, map parseBoard boards)
    where nums:boards = splitOn "\n\n" inp

parseNums :: String -> [Int]
parseNums = map read . splitOn ","

parseBoard :: String -> Board
parseBoard = map (map ((,False) . read) . words) . lines

part1 :: Input -> Output1
part1 = bingo (any checkWin)

bingo :: ([Board] -> Bool) -> ([Int], [Board]) -> Int
bingo _ ([], _) = error "No winner"
bingo winCond (n:nums, boards)
    | winCond newBoards = (n *) $ sum $ map fst $ filter (not . snd) $ concat $ head winners
    | otherwise = bingo winCond (nums, filter (not . checkWin) newBoards)
    where
        newBoards = map (map (\(x, b) -> (x, b || x == n))) <$> boards
        winners = filter checkWin newBoards

checkWin :: Board -> Bool
checkWin rows = winBoard rows || winBoard cols
    where
        cols = transpose rows
        winBoard :: Board -> Bool
        winBoard = any (all snd)

part2 :: Input -> Output2
part2 = bingo (all checkWin)
