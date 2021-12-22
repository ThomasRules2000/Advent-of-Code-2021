module Days.Day11 where
import           Data.Bifunctor  (first)
import           Data.Char       (digitToInt)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Util.Map as Map

runDay :: String -> IO (Maybe Integer, Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int)

type Input = Map Pos Int

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.fromGrid . map (map digitToInt) . lines

part1 :: Input -> Output1
part1 = fst . (!!100) . iterate (\(n, m) -> first (+n) $ doCycle m) . (0,)

doCycle :: Map Pos Int -> (Int, Map Pos Int)
doCycle = getFlashes Set.empty . fmap (+1)

getFlashes :: Set Pos -> Map Pos Int -> (Int, Map Pos Int)
getFlashes flashes octos
    | null newFlashes = (Set.size flashes, Map.fromSet (const 0) flashes Map./\ octos)
    | otherwise = getFlashes (Set.union flashes newFlashes) $ foldl flash octos newFlashes
    where
        newFlashes = Map.keysSet (Map.filter (>9) octos) Set.\\ flashes
        flash m = foldr (Map.adjust (+1)) m . getAround

getAround :: Pos -> [Pos]
getAround (x0, y0) = [(x+x0, y+y0) | x <- [-1..1], y <- [-1..1], x/=0 || y/=0]

part2 :: Input -> Output2
part2 m
    | flashed == Map.size m = 1
    | otherwise = 1 + part2 newMap
    where (flashed, newMap) = doCycle m
