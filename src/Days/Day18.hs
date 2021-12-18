module Days.Day18 where
import           Control.Monad.State
import           Data.Bifunctor      (first, second)
import           Data.Char           (digitToInt, isDigit)
import           Data.Functor        (($>))
import qualified Program.RunDay      as R (runDay)
import           Util.BinTree        (BinTree (..), applyLeft, applyRight,
                                      maxDepth)

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
    | maxDepth sf > 4 = reduce $ evalState (explode 4 sf) $ ExplodeData False Nothing Nothing
    | any (>=10) sf   = reduce $ evalState (split sf) False
    | otherwise       = sf

data ExplodeData = ExplodeData {
    exploded :: Bool,
    lVal     :: Maybe Int,
    rVal     :: Maybe Int
}

explode :: Int -> SnailFish -> State ExplodeData SnailFish
explode _ l@(Leaf _) = return l
explode n sf@(Node (Leaf l) (Leaf r))
    | n <= 0 = put ExplodeData {exploded=True, lVal=Just l, rVal=Just r} $> Leaf 0
    | otherwise = return sf
explode n sf@(Node l r) = do
    newL <- explode (n-1) l
    ExplodeData{..} <- get
    if exploded then
        modify (\ed -> ed{rVal=Nothing})
        $> Node newL (maybe r ((`applyLeft` r) . (+)) rVal)
    else do
        newR <- explode (n-1) r
        ExplodeData{..} <- get
        if exploded then
            modify (\ed -> ed{lVal=Nothing})
            $> Node (maybe l ((`applyRight` l) . (+)) lVal) newR
        else return sf


split :: SnailFish -> State Bool SnailFish
split leaf@(Leaf x)
    | x >= 10 = put True $> Node (floor <$> n) (ceiling <$> n)
    | otherwise = return leaf
    where n = Leaf $ fromIntegral x / 2
split (Node l r) = do
    newL <- split l
    get >>= \case
        True  -> return $ Node newL r
        False -> Node l <$> split r


magnitude :: SnailFish -> Int
magnitude (Leaf x)   = x
magnitude (Node l r) = (3 * magnitude l) + (2 * magnitude r)


part2 :: Input -> Output2
part2 sfs = maximum [magnitude $ addSnailFish l r | l <- sfs, r <- sfs]
