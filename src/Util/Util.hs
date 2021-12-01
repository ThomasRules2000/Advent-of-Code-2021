module Util.Util where
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
