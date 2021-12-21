module Days.Day21 where
import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Functor        (($>))
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Tuple          (swap)
import           Data.Tuple.Extra    (both, first)
import qualified Program.RunDay      as R (runDay)
import           Util.Util           (listToTuple, sumTuples)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Player = (Int, Int)

type Input = (Int, Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = listToTuple . map (read . last . words) . lines

part1 :: Input -> Output1
part1 = uncurry (turn (cycle [1..100]) 0) . both (,0)

turn :: [Int] -> Int -> Player -> Player -> Int
turn die rolls p1@(_, p1Score) p2@(_, p2Score)
    | p2Score >= 1000 = p1Score * rolls
    | otherwise = turn newDie (rolls + 3) p2 $ move p1Roll p1
    where (p1Roll, newDie) = first sum $ splitAt 3 die

move :: Int -> Player -> Player
move spaces (pos, score) = (newPos, score + newPos)
    where newPos = 1 + (pos + spaces - 1) `mod` 10

part2 :: Input -> Output2
part2 = uncurry max . flip evalState mempty . uncurry quantumTurn . both (, 0)

quantumTurn :: Player -> Player -> State (Map (Player, Player) (Int, Int)) (Int, Int)
quantumTurn p1 p2
    | snd p2 >= 21 = return (0,1)
    | otherwise = gets (Map.lookup (p1, p2)) >>= \case
        Just x -> return x
        Nothing -> do
            res <- sumTuples . zipWith (both . (*)) [1,3,6,7,6,3,1] <$> mapM (fmap swap . quantumTurn p2 . flip move p1) [3..9]
            modify (Map.insert (p1, p2) res)
            return res
