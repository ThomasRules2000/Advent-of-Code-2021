module Days.Day10 where
import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (first, second)
import           Data.Either     (lefts, rights)
import           Data.List       (sort)
import           GHC.Generics    (Generic)
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [[Bracket]]

type Output1 = Int
type Output2 = Int

data Bracket = Open  BracketType
             | Close BracketType
             deriving (Eq, Ord, Show, Generic, NFData)

data BracketType = Paren
                 | Square
                 | Brace
                 | Angled
                 deriving (Eq, Ord, Show, Enum, Generic, NFData)

parser :: String -> Input
parser = map (map readBracket) . lines

readBracket :: Char -> Bracket
readBracket '(' = Open  Paren
readBracket ')' = Close Paren
readBracket '[' = Open  Square
readBracket ']' = Close Square
readBracket '<' = Open  Angled
readBracket '>' = Close Angled
readBracket '{' = Open  Brace
readBracket '}' = Close Brace
readBracket  b  = error $ "Invalid bracket: " ++ show b

part1 :: Input -> Output1
part1 = sum . lefts . map (first scoreBracket . firstInvalid)

firstInvalid :: [Bracket] -> Either BracketType [BracketType]
firstInvalid = flip go []
    where
        go [] stack = Right stack
        go (b:bs) stack = case b of
            Open t -> go bs (t:stack)
            Close t
                | t == s -> go bs ss
                | otherwise -> Left t
                where (s:ss) = stack

scoreBracket :: BracketType -> Int
scoreBracket Paren  = 3
scoreBracket Square = 57
scoreBracket Brace  = 1197
scoreBracket Angled = 25137

part2 :: Input -> Output2
part2 = median . rights . map (second scoreStack . firstInvalid)

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

scoreStack :: [BracketType] -> Int
scoreStack = foldl (\acc b -> 5 * acc + fromEnum b + 1) 0
