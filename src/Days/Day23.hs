module Days.Day23 where
import           Data.Bifunctor   (second)
import           Data.Heap        (MinPrioHeap)
import qualified Data.Heap        as Heap
import           Data.List        (find, intersperse, transpose)
import           Data.Maybe       (isNothing)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (both)
import           Data.Vector      (Vector)
import qualified Data.Vector      as Vector
import qualified Program.RunDay   as R (runDay)
import           System.Clock     (TimeSpec)
import           Util.Util        (imap, listToTuple4, setAt, uncurry4)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

data Grid = Grid {
    roomSize :: Int,
    corridor :: [Maybe Amphipod],
    roomA    :: [Amphipod], -- Below space 2
    roomB    :: [Amphipod], -- Below space 4
    roomC    :: [Amphipod], -- Below space 6
    roomD    :: [Amphipod]  -- Below space 8
} deriving (Eq, Ord)

instance Show Grid where
    show Grid{..} = unlines $ ["#############", "#" ++ corridorString ++ "#"] ++ roomStrings ++ ["  #########"]
        where
            corridorString = concatMap (maybe "." show) corridor
            roomStrings = transpose $ [hashSpace, hashSpace, hashes] ++ intersperse hashes (map showRoom [roomA, roomB, roomC, roomD]) ++ [hashes, "#", "#"]

            hashSpace = '#' : replicate (roomSize - 1) ' '
            hashes = replicate roomSize '#'

            showRoom :: [Amphipod] -> String
            showRoom xs  = replicate (roomSize - length xs) '.' ++ concatMap show xs

data Amphipod = A | B | C | D
    deriving (Eq, Ord, Show)

type Input = Grid

type Output1 = Int
type Output2 = Int

data AStarItem = AStarItem {
    gValue :: Int,
    hValue :: Int
} deriving (Eq, Show)

instance Ord AStarItem where
    (AStarItem g1 h1) <= (AStarItem g2 h2) = g1 + h1 <= g2 + h2

fValue :: AStarItem -> Int
fValue AStarItem{..} = gValue + hValue

parser :: String -> Input
parser = uncurry4 (Grid 2 $ replicate 11 Nothing)
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
part1 g = search Set.empty $ Heap.singleton (AStarItem 0 $ getHeuristic g, g)

amphipodMoveCost :: Amphipod -> Int
amphipodMoveCost A = 1
amphipodMoveCost B = 10
amphipodMoveCost C = 100
amphipodMoveCost D = 1000

invalidSpaces :: Set Int
invalidSpaces = Set.fromList [2,4,6,8]

search :: Set Grid -> MinPrioHeap AStarItem Grid -> Int
search closedSet heap
    | isTarget grid = gValue cost
    | Set.member grid closedSet = search closedSet restHeap
    | otherwise = search (Set.insert grid closedSet) $ Heap.union restHeap $ step grid (gValue cost)
    where Just ((cost, grid), restHeap) = Heap.view heap

sameAwayStates :: Grid -> Grid -> Bool
sameAwayStates grid1 grid2 = all (uncurry (==) . (`both` (grid1, grid2))) [roomA, roomB, roomC, roomD]

isTarget :: Grid -> Bool
isTarget Grid{..} = all (==A) roomA && all (==B) roomB && all (==C) roomC && all (==D) roomD && all isNothing corridor

step :: Grid -> Int -> MinPrioHeap AStarItem Grid
step grid@Grid{..} cost = Heap.union roomMoves corridorMoves
    where
        roomMoves = Heap.unions $ map (getRoomMoves grid cost) [A, B, C, D]
        corridorMoves = getCorridorMoves grid cost

getHeuristic :: Grid -> Int
getHeuristic Grid{..} = roomHeuristic + corridorHeuristic
    where
        corridorHeuristic = getCorridorHeuristic corridor
        roomHeuristic = sum $ zipWith (getRoomHeuristic roomSize) [A,B,C,D] [roomA, roomB, roomC, roomD]

getCorridorHeuristic :: [Maybe Amphipod] -> Int
getCorridorHeuristic = sum . imap toMove
    where
        toMove :: Int -> Maybe Amphipod -> Int
        toMove _ Nothing     = 0
        toMove curr (Just a) = amphipodMoveCost a * abs (curr - getDest a)

getRoomHeuristic :: Int -> Amphipod -> [Amphipod] -> Int
getRoomHeuristic maxRoomSize a1 room = removeCost + addCost
    where
        roomSize = length room
        correctRoom = replicate roomSize a1
        Just toRemove = find (\x -> drop x room == drop x correctRoom) [0..roomSize]
        removeCost = sum $ zipWith moveDist [1..] $ take toRemove room

        toAdd = maxRoomSize - roomSize + toRemove
        addCost = amphipodMoveCost a1 * (toAdd * (toAdd + 1)) `div` 2
        roomLoc = getDest a1

        moveDist :: Int -> Amphipod -> Int
        moveDist roomMoves a2 = amphipodMoveCost a2 * (roomMoves + corridorMoves)
            where corridorMoves = if a1 == a2 then 2 else abs (roomLoc - getDest a2)

getCorridorMoves :: Grid -> Int -> MinPrioHeap AStarItem Grid
getCorridorMoves grid@Grid{..} cost = Heap.unions $ imap newCorridor corridor
    where
        newCorridor :: Int -> Maybe Amphipod -> MinPrioHeap AStarItem Grid
        newCorridor _ Nothing = Heap.empty
        newCorridor i (Just a)
            | not canMove = Heap.empty
            | otherwise = Heap.singleton (AStarItem {gValue=newCost, hValue=getHeuristic finalGrid}, finalGrid)
            where
                finalGrid = updatedRoom{corridor=setAt i Nothing corridor}

                dest = getDest a
                canMove = clearPath i dest corridor && checkRoom grid a

                (updatedRoom, newCost) = second (\x -> cost + (amphipodMoveCost a * (abs (i-dest) + x))) $ case a of
                    A -> (grid{roomA=A:roomA}, movesInRoom roomSize roomA)
                    B -> (grid{roomB=B:roomB}, movesInRoom roomSize roomB)
                    C -> (grid{roomC=C:roomC}, movesInRoom roomSize roomC)
                    D -> (grid{roomD=D:roomD}, movesInRoom roomSize roomD)

getRoomMoves :: Grid -> Int -> Amphipod -> MinPrioHeap AStarItem Grid
getRoomMoves grid@Grid{..} cost a
    | checkRoom grid a = Heap.empty
    | otherwise = Heap.fromList $ map getNewGrids $ getReachable start
    where
        getNewGrids :: Int -> (AStarItem, Grid)
        getNewGrids i = (AStarItem {gValue = cost + amphipodMoveCost amphi * (abs (start - i) + movesInRoom roomSize room + 1), hValue = getHeuristic finalGrid}, finalGrid)
            where
                finalGrid = newGrid{corridor=setAt i (Just amphi) corridor}
                amphi = head room

        (start, room, newGrid) = case a of
            A -> (2, roomA, grid{roomA=tail roomA})
            B -> (4, roomB, grid{roomB=tail roomB})
            C -> (6, roomC, grid{roomC=tail roomC})
            D -> (8, roomD, grid{roomD=tail roomD})

        getReachable :: Int -> [Int]
        getReachable start = filter (\i -> i `notElem` invalidSpaces && clearPath start i corridor) [0..10]

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

clearPath :: Int -> Int -> [Maybe Amphipod] -> Bool
clearPath curr dest corridor
    | curr == dest = True -- should never happen
    | otherwise = all isNothing $ take sliceSize $ drop sliceStart corridor
    where
        (sliceStart, sliceSize) = if curr < dest
                                  then (curr + 1, dest - curr)
                                  else (dest, curr - dest)

part2 :: Input -> Output2
part2 g = search Set.empty $ Heap.singleton (AStarItem 0 $ getHeuristic newGrid, newGrid)
    where newGrid = expandGrid g

expandGrid :: Grid -> Grid
expandGrid Grid{..} = Grid{roomSize = 4, corridor=corridor, roomA=insertAt 1 [D, D] roomA, roomB=insertAt 1 [C, B] roomB, roomC=insertAt 1 [B, A] roomC, roomD=insertAt 1 [A, C] roomD}

insertAt :: Int -> [a] -> [a] -> [a]
insertAt n xs ys = take n ys ++ xs ++ drop n ys
