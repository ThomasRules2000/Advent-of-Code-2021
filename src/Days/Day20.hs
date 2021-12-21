module Days.Day20 where
import           Data.Bifunctor  (bimap)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import qualified Program.RunDay  as R (runDay)
import qualified Util.Map        as Map
import           Util.Util       (binToDec, listToTuple)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Pos = (Int, Int)

type Input = (Vector Bool, Set Pos)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (Vector.fromList . map (=='#')) (Map.keysSet . Map.filter (=='#') . Map.fromGrid . lines) . listToTuple . splitOn "\n\n"

part1 :: Input -> Output1
part1 = uncurry (doSteps 2)

getAround :: Pos -> [Pos]
getAround (x0, y0) = [(x+x0, y+y0) | x <- [-1,0,1], y <- [-1,0,1]]

newPixel :: Vector Bool -> Set Pos -> Bool -> Pos -> Bool
newPixel algo nonDefs def pos = (algo Vector.!) $ binToDec $ (def /=) . (`Set.member` nonDefs) <$> getAround pos

step :: Vector Bool -> Set Pos -> Bool -> (Set Pos, Bool)
step algo nonDefs def = (newNonDefs, newDef)
    where
        xs = Set.map fst nonDefs
        ys = Set.map snd nonDefs
        newNonDefs = Set.filter ((newDef /=) . newPixel algo nonDefs def) 
                   $ Set.fromList [(x,y) | x <- [Set.findMin xs-1..Set.findMax xs+1], y <- [Set.findMin ys-1..Set.findMax ys+1]]
        newDef = if def
            then Vector.last algo
            else Vector.head algo

doSteps :: Int -> Vector Bool -> Set Pos -> Int
doSteps n algo = Set.size . fst . (!!n) . iterate (uncurry (step algo)) . (, False)

part2 :: Input -> Output2
part2 = uncurry (doSteps 50)
