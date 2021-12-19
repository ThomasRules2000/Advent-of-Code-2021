module Util.Map where
import           Data.Bifunctor  (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

(\/) :: Ord k => Map k v -> Map k v -> Map k v
(\/) = Map.union

(/\) :: Ord k => Map k v -> Map k v -> Map k v
(/\) = Map.intersection

containsKeys :: Ord k => Map k v -> [k] -> Bool
containsKeys m = all (`Map.member` m)

gridToMap :: [[a]] -> Map (Int, Int) a
gridToMap = Map.fromList
       . concat
       . zipWith (\x -> map $ first (x,)) [0..]
       . map (zip [0..])
