module Days.Day25 where
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Program.RunDay  as R (runDay)
import           System.Clock    (TimeSpec)
import qualified Util.Map        as Map
import           Util.NoQuotes   (NoQuotes(..))

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int)

type Input = Map Pos (Maybe Cucumber)

type Output1 = Int
type Output2 = NoQuotes 

data Cucumber = East | South
    deriving (Eq, Ord, Show)

parser :: String -> Input
parser = Map.fromGrid . map (map parseCucumber) . lines

parseCucumber :: Char -> Maybe Cucumber
parseCucumber 'v' = Just South
parseCucumber '>' = Just East
parseCucumber  _  = Nothing

part1 :: Input -> Output1
part1 m = moveCucumbers maxPos m
    where maxPos = fst $ Map.findMax m

moveCucumbers :: Pos -> Map Pos (Maybe Cucumber) -> Int
moveCucumbers maxPos m
    | Map.null canMoveEast && Map.null canMoveSouth = 1
    | otherwise = 1 + moveCucumbers maxPos movedSouth
    where
        canMoveEast = Map.filterWithKey (\k v -> canMove maxPos m k v && v == Just East) m
        movedEast = Map.union (moveMap canMoveEast) m
        canMoveSouth = Map.filterWithKey (\k v -> canMove maxPos movedEast k v && v == Just South) movedEast
        movedSouth = Map.union (moveMap canMoveSouth) movedEast

        moveMap :: Map Pos (Maybe Cucumber) -> Map Pos (Maybe Cucumber)
        moveMap m = Map.union (Nothing <$ m) $ Map.mapKeys (newPosMap Map.!) m
            where newPosMap = Map.mapWithKey (nextLocation maxPos) m

canMove :: Pos -> Map Pos (Maybe Cucumber) -> Pos -> Maybe Cucumber -> Bool
canMove _ _ _ Nothing = False
canMove maxPos m p c  = (==Nothing) $ (m Map.!) $ nextLocation maxPos p c

nextLocation :: Pos -> Pos -> Maybe Cucumber -> Pos
nextLocation (xMax, _) (x, y) (Just South)
    | x == xMax = (0, y)
    | otherwise = (x+1, y)
nextLocation (_, yMax) (x, y) (Just East)
    | y == yMax = (x, 0)
    | otherwise = (x, y+1)
nextLocation _ (x, y) Nothing = (x, y)

part2 :: Input -> Output2
part2 = const $ NoQuotes "Merry Christmas!"
