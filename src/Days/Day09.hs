module Days.Day09 where
import           Data.Bifunctor  (first)
import           Data.Char       (digitToInt)
import           Data.Foldable   (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import           Util.Util       (gridToMap)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int)

type Input = Map Pos Int

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = gridToMap
       . map (map digitToInt)
       . lines

part1 :: Input -> Output1
part1 mat = sum filtered + Map.size filtered
    where filtered = Map.filterWithKey (isLowest mat) mat

isLowest :: Map Pos Int -> Pos -> Int -> Bool
isLowest mat pos n = all (>n) $ catMaybes $ flip Map.lookup mat <$> getAdjacent pos

getAdjacent :: Pos -> [Pos]
getAdjacent (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

part2 :: Input -> Output2
part2 = multTop3 . Set.map Set.size . Map.foldrWithKey foldFunc Set.empty
    where
        multTop3 :: Set Int -> Int
        multTop3 s0 = let (n1, s1) = Set.deleteFindMax s0 in
            let (n2, s2) = Set.deleteFindMax s1 in
                let (n3, s3) = Set.deleteFindMax s2 in
                    n1 * n2 * n3

        foldFunc :: Pos -> Int -> Set (Set Pos) -> Set (Set Pos)
        foldFunc pos height basins
            | height >= 9 = basins
            | otherwise = Set.insert (Set.insert pos $ Set.unions sets) $ basins Set.\\ sets
            where sets = Set.fromList $ mapMaybe (\x -> find (Set.member x) basins) $ getAdjacent pos
