module Days.Day04 where
import           Data.Bifunctor  (bimap)
import           Data.List       (transpose, uncons)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = ([Int], [Board])

type Output1 = Int
type Output2 = Int

type Board = [[(Int, Bool)]]

parser :: String -> Input
parser = bimap parseNums (map parseBoard) . fromJust . uncons . splitOn "\n\n"

parseNums :: String -> [Int]
parseNums = map read . splitOn ","

parseBoard :: String -> Board
parseBoard = map (map ((,False) . read) . words) . lines

part1 :: Input -> Output1
part1 = bingo (any checkWin)

bingo :: ([Board] -> Bool) -> ([Int], [Board]) -> Int
bingo _ ([], _) = error "No winner"
bingo stopCond (n:nums, boards)
    | stopCond newBoards = (n *) $ sum $ map fst $ filter (not . snd) $ concat $ head winners
    | otherwise          = bingo stopCond (nums, filter (not . checkWin) newBoards)
    where
        newBoards = map (map (\(x, b) -> (x, b || x == n))) <$> boards
        winners   = filter checkWin newBoards

checkWin :: Board -> Bool
checkWin rows = winBoard rows || winBoard cols
    where
        cols = transpose rows
        winBoard :: Board -> Bool
        winBoard = any (all snd)

part2 :: Input -> Output2
part2 = bingo (all checkWin)
