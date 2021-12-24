module Days.Day24 where
import           Data.Foldable   (foldl')
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import qualified Program.RunDay  as R (runDay)
import           System.Clock    (TimeSpec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type Input = [Instruction]

type Output1 = Int
type Output2 = Int

data Instruction = Inp Var
                 | Add Var (Either Var Int)
                 | Mul Var (Either Var Int)
                 | Div Var (Either Var Int)
                 | Mod Var (Either Var Int)
                 | Eql Var (Either Var Int)
                 deriving (Eq, Ord, Show)

data Var = W | X | Y | Z
    deriving (Eq, Ord, Show, Enum)

parser :: String -> Input
parser = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction s = case op of
    "inp" -> Inp a
    "add" -> Add a b
    "mul" -> Mul a b
    "div" -> Div a b
    "mod" -> Mod a b
    "eql" -> Eql a b
    _     -> error $ "Invalid operation: " ++ op
    where
        (op:v1:vars) = words s
        (v2:_) = vars
        Left a = parseVar v1
        b = parseVar v2

parseVar :: String -> Either Var Int
parseVar "w" = Left W
parseVar "x" = Left X
parseVar "y" = Left Y
parseVar "z" = Left Z
parseVar  i  = Right $ read i

part1 :: Input -> Output1
part1 = getAns Map.findMax

getAns :: (Map (Int, Int) Int -> ((Int, Int), Int)) -> [Instruction] -> Int
getAns f is = foldl' (\acc x -> 10*acc + x) 0 $ findMaxValid f $ snd $ findValidInputs (Set.singleton 0) chunks
    where chunks = tail $ splitOn [Inp W, Mul X (Right 0), Add X (Left Z), Mod X (Right 26)] is

findMaxValid :: (Map (Int, Int) Int -> ((Int, Int), Int)) -> [Map (Int, Int) Int] -> [Int]
findMaxValid f [] = []
findMaxValid f [m] = [fst $ fst $ f m]
findMaxValid f (m0:m1:ms) = maxW : findMaxValid f (filteredM1 : ms)
    where
        maxW = fst $ fst $ f m0
        validZs = Set.fromList $ Map.elems $ Map.filterWithKey (\k v -> fst k == maxW) m0
        filteredM1 = Map.filterWithKey (\k v -> snd k `Set.member` validZs) m1

findValidInputs :: Set Int -> [[Instruction]] -> (Set Int, [Map (Int, Int) Int])
findValidInputs _ [] = (Set.singleton 0, [])
findValidInputs zs (is:iss) = (Set.map snd $ Map.keysSet validWZs, validWZs : inps)
    where
       inOutMap = doStep zs is
       (newZs, inps) = findValidInputs (Set.fromList $ Map.elems inOutMap) iss
       validWZs = Map.filter (`Set.member` newZs) inOutMap


doStep :: Set Int -> [Instruction] -> Map (Int, Int) Int
doStep outputZs is = Map.fromList $ zip pairs $ map (\(w, z) -> (Vector.! 3) $ foldl evalInstruction (Vector.fromList [w, z `mod` 26, 0, z]) is) pairs
    where
        pairs = filter filterXEq [(w, z) | z <- Set.toList outputZs, w <- [1..9]]
        Add X (Right v2) = is !! 1

        filterXEq :: (Int, Int) -> Bool
        filterXEq (w, z)
            | v2 <= 9   = w == (z `mod` 26) + v2
            | otherwise = True

evalInstruction :: Vector Int -> Instruction -> Vector Int
evalInstruction vars (Inp a) = error "Tried to take input" -- vars Vector.// [(fromEnum a, inp)]
evalInstruction vars i = vars Vector.// case i of
    Add a b -> [(fromEnum a, getVal1 a + getVal2 b)]
    Mul a b -> [(fromEnum a, getVal1 a * getVal2 b)]
    Div a b -> [(fromEnum a, getVal1 a `div` getVal2 b)]
    Mod a b -> [(fromEnum a, getVal1 a `mod` getVal2 b)]
    Eql a b -> [(fromEnum a, fromEnum $ getVal1 a == getVal2 b)]
    Inp _   -> error "Implemented"
    where
        getVal1 :: Var -> Int
        getVal1 v = vars Vector.! fromEnum v
        getVal2 :: Either Var Int -> Int
        getVal2 (Right i) = i
        getVal2 (Left v)  = getVal1 v

part2 :: Input -> Output2
part2 = getAns Map.findMin
