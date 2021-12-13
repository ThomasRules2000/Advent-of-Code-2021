module Days.Day12 where
import           Control.DeepSeq (NFData)
import           Data.Char       (isUpper)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Tuple      (swap)
import           GHC.Generics    (Generic)
import qualified Program.RunDay  as R (runDay)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = Map Cave [Cave]

type Output1 = Int
type Output2 = Int

data Cave = Start
          | End
          | Small String
          | Big String
          deriving (Eq, Ord, Show, Generic, NFData)

parser :: String -> Input
parser = Map.fromListWith (++)
       . map (fmap pure)
       . concatMap ((\t -> [t, swap t]) . listToTuple . map parseCave . splitOn "-")
       . lines

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end"   = End
parseCave s       = if isUpper $ head s
                    then Big s
                    else Small s

part1 :: Input -> Output1
part1 graph = Set.size $ navigate graph True [] Start

part2 :: Input -> Output2
part2 graph = Set.size $ navigate graph False [] Start

navigate :: Map Cave [Cave] -> Bool -> [Cave] -> Cave -> Set [Cave]
navigate graph smallTwice visited pos
    | pos == End = Set.singleton (pos:visited)
    | otherwise = Set.unions $ map (navigate graph newSmallTwice (pos:visited)) canVisit
    where
        canVisit = filter visitable $ graph Map.! pos
        newSmallTwice = smallTwice || (isSmall pos && pos `elem` visited)
        visitable :: Cave -> Bool
        visitable (Big _)     = True
        visitable End         = True
        visitable Start       = False
        visitable s@(Small _)
            | newSmallTwice = s `notElem` visited
            | otherwise  = length (filter (==s) visited) < 2

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _         = False
