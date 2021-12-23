module Days.Day02 where
import           Data.Bifunctor (first, second)
import           Data.Foldable  (foldl')
import qualified Program.RunDay as R (runDay)
import           System.Clock   (TimeSpec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

data Move = Forward Int
          | Down Int
          deriving (Eq, Ord, Show)

type Input = [Move]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (getMove . words) . lines

getMove :: [String] -> Move
getMove [dir, num] = case dir of
    "forward" -> Forward $ read num
    "down"    -> Down $ read num
    "up"      -> Down $ negate $ read num
    _         -> error "Not one of \"forward\", \"down\" or \"up\""
getMove _ = error "Too many words"


part1 :: Input -> Output1
part1 = uncurry (*) . foldl' (flip updatePosition) (0,0)

updatePosition :: Move -> (Int, Int) -> (Int, Int)
updatePosition (Forward n) = first  (+n)
updatePosition (Down n)    = second (+n)


part2 :: Input -> Output2
part2 = uncurry (*) . snd . foldl' (flip updatePosition2) (0, (0, 0))

updatePosition2 :: Move -> (Int, (Int, Int)) -> (Int, (Int, Int))
updatePosition2 (Down n)    (aim, pos)    = (aim + n, pos)
updatePosition2 (Forward n) (aim, (x, y)) = (aim, (x + n, y + (aim * n)))
