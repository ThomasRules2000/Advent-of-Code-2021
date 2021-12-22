module Days.Day13 where
import           Data.Bifunctor  (bimap, first, second)
import           Data.Foldable   (foldl')
import           Data.List.Split (splitOn)
import           Data.Matrix     (Matrix)
import qualified Data.Matrix     as Matrix
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Tuple      (swap)
import qualified Program.RunDay  as R (runDay)
import           Util.NoQuotes   (NoQuotes (NoQuotes))
import           Util.Util       (listToTuple, ppMatrix)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int)

type Input = (Set Pos, [Fold])

type Output1 = Int
type Output2 = NoQuotes

data Fold = X Int
          | Y Int
          deriving (Eq, Ord, Show)

parser :: String -> Input
parser = bimap (Set.fromList . map (listToTuple . map read . splitOn ",") . lines) (map (parseFold . last . words) . lines) . listToTuple . splitOn "\n\n"

parseFold :: String -> Fold
parseFold (dir:eq:num) = case dir of
    'x' -> X $ read num
    'y' -> Y $ read num
    _   -> error "Invalid axis for fold"
parseFold _ = error "Invalid fold"

part1 :: Input -> Output1
part1 (points, f:_) = Set.size $ fold f points

fold :: Fold -> Set Pos -> Set Pos
fold = \case
    X x -> Set.map (first  (doFold x))
    Y y -> Set.map (second (doFold y))
    where
        doFold :: Int -> Int -> Int
        doFold f n
            | n <= f    = n
            | otherwise = (2 * f) - n

part2 :: Input -> Output2
part2 (points, folds) = NoQuotes $ ppMatrix $ Matrix.matrix x y (`Set.member` finalPoints)
    where
        finalPoints = Set.map (swap . bimap (+1) (+1)) $ foldl' (flip fold) points folds
        x = Set.findMax $ Set.map fst finalPoints
        y = Set.findMax $ Set.map snd finalPoints

