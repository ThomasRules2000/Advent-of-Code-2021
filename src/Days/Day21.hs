module Days.Day21 where
import           Control.Monad.ST            (runST)
import           Data.Tuple.Extra            (both, first, swap)
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Program.RunDay              as R (runDay)
import           Util.Util                   (listToTuple, sumTuples)

runDay :: String -> IO (Maybe Integer, Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Player = (Int, Int)

type Input = (Player, Player)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = both (,0) . listToTuple . map (read . last . words) . lines

part1 :: Input -> Output1
part1 = uncurry (turn (cycle [1..100]) 0)

turn :: [Int] -> Int -> Player -> Player -> Int
turn die rolls p1 p2
    | snd p2 >= 1000 = snd p1 * rolls
    | otherwise = turn newDie (rolls + 3) p2 $ move p1Roll p1
    where (p1Roll, newDie) = first sum $ splitAt 3 die

move :: Int -> Player -> Player
move spaces (pos, score) = (newPos, score + newPos)
    where newPos = 1 + (pos + spaces - 1) `mod` 10

part2 :: Input -> Output2
part2 = uncurry max . quantumTurn

quantumTurn :: (Player, Player) -> (Int, Int)
quantumTurn (p1, p2) = runST $ do
    knowns <- MVector.replicate 44100 (-1, -1)
    let go p1 p2
            | snd p2 >= 21 = return (0,1)
            | otherwise = MVector.unsafeRead knowns i >>= \case
                (-1, _) -> do
                    res <- sumTuples . zipWith (both . (*)) [1,3,6,7,6,3,1] <$> mapM (fmap swap . go p2 . flip move p1) [3..9]
                    MVector.unsafeWrite knowns i res
                    return res
                t -> return t
            where i = vecIndex p1 p2
    go p1 p2

vecIndex :: Player -> Player -> Int
vecIndex (p1Pos, p1Score) (p2Pos, p2Score) = (p1Pos - 1) * 21 * 21 * 10
                                           + (p2Pos - 1) * 21 * 21
                                           +  p1Score    * 21
                                           +  p2Score
