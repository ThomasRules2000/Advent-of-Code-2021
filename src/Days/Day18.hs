module Days.Day18 where
import           Data.Bifunctor (first)
import           Data.Char      (digitToInt, isDigit)
import qualified Program.RunDay as R (runDay)
import           Util.BinTree   (BinTree (..), maxDepth, applyLeft, applyRight)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [SnailFish]

type Output1 = Int
type Output2 = Int

type SnailFish = BinTree Int

parser :: String -> Input
parser = map (fst . parseSnailFish) . lines

parseSnailFish :: String -> (SnailFish, String)
parseSnailFish (s:ss) = case s of
    '[' -> (Node lSF rSF, rRest)
    c   | isDigit c -> (Leaf $ digitToInt c, ss)
        | otherwise -> error $ "Invalid character: " ++ show c
    where
        (lSF, _:lRest) = parseSnailFish ss
        (rSF, _:rRest) = parseSnailFish lRest
parseSnailFish [] = error "Invalid parse"


part1 :: Input -> Output1
part1 = magnitude . foldl1 addSnailFish

addSnailFish :: SnailFish -> SnailFish -> SnailFish
addSnailFish l r = reduce $ Node l r

reduce :: SnailFish -> SnailFish
reduce sf
    | maxDepth sf > 4 = reduce $ fst $ explode 4 sf
    | any (>=10) sf   = reduce $ fst $ split sf
    | otherwise       = sf

explode :: Int -> SnailFish -> (SnailFish, (Bool, (Maybe Int, Maybe Int)))
explode _ l@(Leaf _) = (l, (False, (Nothing, Nothing)))
explode n sf@(Node (Leaf l) (Leaf r))
    | n <= 0    = (Leaf 0, (True,  (Just l,  Just r)))
    | otherwise = (sf,     (False, (Nothing, Nothing)))
explode n sf@(Node l r)
    | leftExp   = (Node newL (maybe r ((`applyLeft` r) . (+)) lRAdd),  (True,  (lLAdd,   Nothing)))
    | rightExp  = (Node (maybe l ((`applyRight` l) . (+)) rLAdd) newR, (True,  (Nothing, rRAdd  )))
    | otherwise = (sf,                                                 (False, (Nothing, Nothing)))
    where
        (newL, (leftExp,  (lLAdd, lRAdd))) = explode (n-1) l
        (newR, (rightExp, (rLAdd, rRAdd))) = explode (n-1) r

split :: SnailFish -> (SnailFish, Bool)
split leaf@(Leaf x)
    | x >= 10 = (,True) $ if even x then Node l l else Node l r
    | otherwise = (leaf, False)
    where
        l = Leaf $ x `div` 2
        r = Leaf $ 1 + (x `div` 2)
split (Node l r) = case split l of
    (sf, True) -> (Node sf r, True)
    _          -> first (Node l) $ split r

magnitude :: SnailFish -> Int
magnitude (Leaf x)   = x
magnitude (Node l r) = (3 * magnitude l) + (2 * magnitude r)


part2 :: Input -> Output2
part2 sfs = maximum [magnitude $ addSnailFish l r | l <- sfs, r <- sfs]
