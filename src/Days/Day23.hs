module Days.Day23 where
import           Data.Bifunctor
import           Data.Heap        (MinPrioHeap)
import qualified Data.Heap        as Heap
import           Data.List
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (isNothing)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (both)
import           Data.Vector      (Vector)
import qualified Data.Vector      as Vector
import qualified Program.RunDay   as R (runDay)
import           System.Clock     (TimeSpec)
import           Util.Util

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

data Grid = Grid {
    corridor :: Vector (Maybe Amphipod),
    roomA    :: [Amphipod], -- Below space 2
    roomB    :: [Amphipod], -- Below space 4
    roomC    :: [Amphipod], -- Below space 6
    roomD    :: [Amphipod]  -- Below space 8
} deriving (Eq, Ord)

instance Show Grid where
    show Grid{..} = unlines $ ["#############", "#" ++ corridorString ++ "#"] ++ roomStrings ++ ["  #########"]
        where
            corridorString = concatMap (maybe "." show) $ Vector.toList corridor
            roomStrings = transpose $ ["# ", "# ", "##"] ++ intersperse "##" (map showRoom [roomA, roomB, roomC, roomD]) ++ ["##", "#", "#"]

            showRoom :: [Amphipod] -> String
            showRoom [x] = '.':show x
            showRoom xs  = concatMap show xs

data Amphipod = A | B | C | D
    deriving (Eq, Ord, Show)

type Input = Grid

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = uncurry4 (Grid $ Vector.replicate 11 Nothing)
       . listToTuple4
       . map (map getAmphipod)
       . filter ((/= '#'). head)
       . transpose
       . init
       . drop 2
       . lines

getAmphipod :: Char -> Amphipod
getAmphipod 'A' = A
getAmphipod 'B' = B
getAmphipod 'C' = C
getAmphipod 'D' = D
getAmphipod  _  = error "Not an amphipod"

part1 :: Input -> Output1
part1 = search 2 (getTarget 2) Set.empty . Heap.singleton . (0,)

amphipodMoveCost :: Amphipod -> Int
amphipodMoveCost A = 1
amphipodMoveCost B = 10
amphipodMoveCost C = 100
amphipodMoveCost D = 1000

invalidSpaces :: Set Int
invalidSpaces = Set.fromList [2,4,6,8]

search :: Int -> Grid -> Set Grid -> MinPrioHeap Int Grid -> Int
search roomSize target closedSet heap
    | grid == target = cost
    | Set.member grid closedSet = search roomSize target closedSet restHeap
    | otherwise = search roomSize target (Set.insert grid closedSet) $ Heap.union restHeap $ step roomSize grid cost
    where Just ((cost, grid), restHeap) = Heap.view heap

sameAwayStates :: Grid -> Grid -> Bool
sameAwayStates grid1 grid2 = all (uncurry (==) . (`both` (grid1, grid2))) [roomA, roomB, roomC, roomD]

getTarget :: Int -> Grid
getTarget n = Grid{corridor=Vector.replicate 11 Nothing, roomA=replicate n A, roomB=replicate n B, roomC=replicate n C, roomD=replicate n D}

step :: Int -> Grid -> Int -> MinPrioHeap Int Grid
step roomSize grid@Grid{..} cost = Heap.union roomMoves corridorMoves
    where
        roomMoves = Heap.unions $ map (getRoomMoves roomSize grid cost) [A, B, C, D]
        corridorMoves = getCorridorMoves roomSize grid cost

getCorridorMoves :: Int -> Grid -> Int -> MinPrioHeap Int Grid
getCorridorMoves roomSize grid@Grid{..} cost = Heap.unions $ Vector.toList $ Vector.imap newCorridor corridor
    where
        newCorridor :: Int -> Maybe Amphipod -> MinPrioHeap Int Grid
        newCorridor _ Nothing = Heap.empty
        newCorridor i (Just a)
            | not canMove = Heap.empty
            | otherwise = Heap.singleton (newCost, updatedRoom{corridor=corridor Vector.// [(i, Nothing)]})
            where
                dest = getDest a
                canMove = clearPath i dest corridor && checkRoom grid a

                (updatedRoom, newCost) = second (\x -> cost + (amphipodMoveCost a * (abs (i-dest) + x))) $ case a of
                    A -> (grid{roomA=A:roomA}, movesInRoom roomSize roomA)
                    B -> (grid{roomB=B:roomB}, movesInRoom roomSize roomB)
                    C -> (grid{roomC=C:roomC}, movesInRoom roomSize roomC)
                    D -> (grid{roomD=D:roomD}, movesInRoom roomSize roomD)

getRoomMoves :: Int -> Grid -> Int -> Amphipod -> MinPrioHeap Int Grid
getRoomMoves roomSize grid@Grid{..} cost a
    | checkRoom grid a = Heap.empty
    | otherwise = Heap.fromList $ map getNewGrids $ getReachable start
    where
        getNewGrids :: Int -> (Int, Grid)
        getNewGrids i = (cost + amphipodMoveCost amphi * (abs (start - i) + movesInRoom roomSize room + 1), newGrid{corridor=corridor Vector.// [(i, Just amphi)]})
            where amphi = head room

        (start, room, newGrid) = case a of
            A -> (2, roomA, grid{roomA=tail roomA})
            B -> (4, roomB, grid{roomB=tail roomB})
            C -> (6, roomC, grid{roomC=tail roomC})
            D -> (8, roomD, grid{roomD=tail roomD})

        getReachable :: Int -> [Int]
        getReachable start = {- getReachableLeft start ++ getReachableRight start -} filter (\i -> i `notElem` invalidSpaces && clearPath start i corridor) [0..10]

        getReachableLeft :: Int -> [Int]
        getReachableLeft i
            | i < 0 = []
            | otherwise = case corridor Vector.! i of
                Just _  -> []
                Nothing -> if i `elem` invalidSpaces then next else i:next
            where next = getReachableLeft (i-1)

        getReachableRight :: Int -> [Int]
        getReachableRight i
            | i > 10 = []
            | otherwise = case corridor Vector.! i of
                Just _  -> []
                Nothing -> if i `elem` invalidSpaces then next else i:next
            where next = getReachableRight (i+1)

movesInRoom :: Int -> [Amphipod] -> Int
movesInRoom roomSize = (roomSize -) . length

checkRoom :: Grid -> Amphipod -> Bool
checkRoom Grid{..} A = all (==A) roomA
checkRoom Grid{..} B = all (==B) roomB
checkRoom Grid{..} C = all (==C) roomC
checkRoom Grid{..} D = all (==D) roomD

getDest :: Amphipod -> Int
getDest A = 2
getDest B = 4
getDest C = 6
getDest D = 8

clearPath :: Int -> Int -> Vector (Maybe Amphipod) -> Bool
clearPath curr dest corridor
    | curr == dest = True -- should never happen
    | otherwise = all isNothing $ Vector.slice sliceStart sliceSize corridor
    where
        (sliceStart, sliceSize) = if curr < dest
                                  then (curr + 1, dest - curr - 1)
                                  else (dest, curr - dest - 1)

part2 :: Input -> Output2
part2 = search 4 (getTarget 4) Set.empty . Heap.singleton . (0,) . expandGrid

expandGrid :: Grid -> Grid
expandGrid Grid{..} = Grid{corridor=corridor, roomA=insertAt 1 [D, D] roomA, roomB=insertAt 1 [C, B] roomB, roomC=insertAt 1 [B, A] roomC, roomD=insertAt 1 [A, C] roomD}

insertAt :: Int -> [a] -> [a] -> [a]
insertAt n xs ys = take n ys ++ xs ++ drop n ys
