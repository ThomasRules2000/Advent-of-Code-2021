module Util.Util where
import           Data.Bifunctor  (first)
import           Data.Foldable   (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

listToTuple :: [a] -> (a,a)
listToTuple [x,y] = (x,y)

containsKeys :: Ord k => Map k v -> [k] -> Bool
containsKeys m = all (`Map.member` m)

insertAfter :: Eq a => a -> [a] -> [a] -> [a]
insertAfter _ toInsert [] = toInsert
insertAfter after toInsert (l:ls)
  | l == after = l:(toInsert ++ ls)
  | otherwise  = l:insertAfter after toInsert ls

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- binToDec :: [Bool] -> Int
-- binToDec = go . reverse
--     where
--         go :: [Bool] -> Int
--         go []     = 0
--         go (x:xs) = fromEnum x + 2 * go xs

-- binToDec :: [Bool] -> Int
-- binToDec = sum . map fst . filter snd . zip [2^x | x <-[0..]] . reverse

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> 2*acc + fromEnum b) 0

gridToMap :: [[a]] -> Map (Int, Int) a
gridToMap = Map.fromList
       . concat
       . zipWith (\x -> map $ first (x,)) [0..]
       . map (zip [0..])
