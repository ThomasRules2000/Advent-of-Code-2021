module Days.Day14 where
import           Data.Bifunctor  (bimap)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = (String, Map CharPair (Set CharPair))

type Output1 = Int
type Output2 = Int

type CharPair = (Char, Char)

parser :: String -> Input
parser = fmap ( Map.mapWithKey getExps 
              . Map.fromList 
              . map (bimap listToTuple head . listToTuple  . splitOn " -> ") 
              . lines)
       . listToTuple
       . splitOn "\n\n"

getExps :: CharPair -> Char -> Set CharPair
getExps (c1, c3) c2 = Set.fromList [(c1, c2), (c2, c3)]

part1 :: Input -> Output1
part1 = ansAfterIters 10

step :: Map CharPair (Set CharPair) -> Map CharPair Int -> Map CharPair Int
step exps = Map.unionsWith (+) . Map.elems . Map.mapWithKey expand
    where
        expand :: CharPair -> Int -> Map CharPair Int
        expand tup n = maybe (Map.singleton tup n) (Map.fromSet (const n)) $ Map.lookup tup exps

ansAfterIters :: Int -> Input -> Int
ansAfterIters n (inp, reps) = maximum end - minimum end
    where end = Map.insertWith (+) (last inp) 1 
              $ Map.mapKeysWith (+) fst 
              $ (!! n) 
              $ iterate (step reps) 
              $ Map.fromListWith (+) 
              $ map (, 1) 
              $ zip inp $ tail inp

part2 :: Input -> Output2
part2 = ansAfterIters 40
