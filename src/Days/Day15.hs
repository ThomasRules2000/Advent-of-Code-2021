module Days.Day15 where
import           Data.Bifunctor  (bimap, first, second)
import           Data.Char       (digitToInt)
import           Data.Foldable   (foldr')
import           Data.Heap       (MinPrioHeap)
import qualified Data.Heap       as Heap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Tuple      (swap)
import qualified Program.RunDay  as R (runDay)
import qualified Util.Map        as Map

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int)

type Input = Map Pos Int

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.gridToMap . map (map digitToInt) . lines

part1 :: Input -> Output1
part1 = runDijkstra

getAdjacent :: Pos -> [Pos]
getAdjacent (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

inRange :: Pos -> Pos -> Pos -> Bool
inRange (x0, y0) (x1, y1) (x,y) = and [x0 <= x, x <= x1, y0 <= y, y <= y1]

runDijkstra :: Map Pos Int -> Int
runDijkstra weights = dijkstra weights (Set.findMax $ Map.keysSet weights) (Heap.singleton (0, (0,0))) Set.empty

dijkstra :: Map Pos Int -> Pos -> MinPrioHeap Int Pos -> Set Pos  -> Int
dijkstra weights destination heap visited
    | currentPos == destination = currentWeight
    | otherwise = dijkstra weights destination newHeap (Set.insert currentPos visited)
    where
        Just ((currentWeight, currentPos), tailHeap) = Heap.view heap
        adjacent = filter isValid $ getAdjacent currentPos
        (neighbours, restHeap) = Heap.partition ((`elem` adjacent) . snd) tailHeap
        newHeap = foldr' (Heap.insert . newWeight (Map.fromList $ map swap $ Heap.toList neighbours)) restHeap adjacent

        isValid :: Pos -> Bool
        isValid p = p `Set.notMember` visited && inRange (0,0) destination p

        newWeight :: Map Pos Int -> Pos -> (Int, Pos)
        newWeight m p = (min newW $ Map.findWithDefault newW p m, p)
            where newW = weights Map.! p + currentWeight

newtype AStarItem = AStarItem (Int, Int)
    deriving (Eq, Show)
instance Ord AStarItem where
    (AStarItem (g1, h1)) <= (AStarItem (g2, h2)) = (g1 + h1) <= (g2 + h2)

getG :: AStarItem -> Int
getG (AStarItem (g, _)) = g

runAStar :: Map Pos Int -> Int
runAStar weights = aStar weights end (Heap.singleton (AStarItem (0, uncurry (+) end), (0,0))) Set.empty
    where end = Set.findMax $ Map.keysSet weights

aStar :: Map Pos Int -> Pos -> MinPrioHeap AStarItem Pos -> Set Pos -> Int
aStar weights destination heap visited
    | currentPos == destination = currG
    | otherwise = aStar weights destination newHeap (Set.insert currentPos visited)
    where
        Just ((AStarItem (currG, currH), currentPos), tailHeap) = Heap.view heap
        adjacent = filter isValid $ getAdjacent currentPos
        (neighbours, restHeap) = Heap.partition ((`elem` adjacent) . snd) tailHeap
        newHeap = foldr' (Heap.insert . newWeight (map swap $ Heap.toList neighbours)) restHeap adjacent

        isValid :: Pos -> Bool
        isValid p = p `Set.notMember` visited && inRange (0,0) destination p

        newWeight :: [(Pos, AStarItem)] -> Pos -> (AStarItem, Pos)
        newWeight m p@(x,y) = (,p) $ case lookup p m of
            Just (AStarItem (g, h)) -> AStarItem (min g newG, h)
            Nothing -> AStarItem (newG, abs (x - destX) + abs (y - destY))
            where
                newG = weights Map.! p + currG
                (destX, destY) = destination

part2 :: Input -> Output2
part2 = runDijkstra . genNewMap

genNewMap :: Map Pos Int -> Map Pos Int
genNewMap tile = Map.unions
               $ take 5
               $ iterate (fmap add1Mod10 . Map.mapKeys (second (+ySize)))
               $ Map.unions
               $ take 5
               $ iterate (fmap add1Mod10 . Map.mapKeys (first (+xSize))) tile
    where
        (xSize, ySize) = bimap (+1) (+1) $ maximum $ Map.keysSet tile
        add1Mod10 x
            | x == 9 = 1
            | otherwise = x + 1
