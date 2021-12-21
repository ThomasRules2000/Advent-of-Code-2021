module Days.Day21 where
import           Data.Tuple       (swap)
import           Data.Tuple.Extra (both, first)
import qualified Program.RunDay   as R (runDay)
import           Util.Util        (listToTuple, sumTuples)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = (Int, Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = listToTuple . map (read . last . words) . lines

part1 :: Input -> Output1
part1 = uncurry (turn (cycle [1..100]) 0) . both (,0)

turn :: [Int] -> Int -> (Int, Int) -> (Int, Int) -> Int
turn die rolls p1@(_, p1Score) p2@(_, p2Score)
    | p2Score >= 1000 = p1Score * rolls
    | otherwise = turn newDie (rolls + 3) p2 $ uncurry (move p1Roll) p1
    where (p1Roll, newDie) = first sum $ splitAt 3 die

move :: Int -> Int -> Int -> (Int, Int)
move spaces pos score = (newPos, score + newPos)
    where newPos = 1 + (pos + spaces - 1) `mod` 10

part2 :: Input -> Output2
part2 = uncurry max . uncurry turn' . both (, 0)

turn' :: (Int, Int) -> (Int, Int) -> (Int, Int)
turn' p1 p2@(_, p2Score)
    | p2Score >= 21 = (0, 1)
    | otherwise = sumTuples $ zipWith (both . (*)) [1,3,6,7,6,3,1] $ map (swap . turn' p2 . \x -> uncurry (move x) p1) [3..9]
