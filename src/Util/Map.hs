module Util.Map where
import           Data.Bifunctor  (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Infix `union`
(\/) :: Ord k => Map k v -> Map k v -> Map k v
(\/) = Map.union

-- | Infix `intersection`
(/\) :: Ord k => Map k v -> Map k v -> Map k v
(/\) = Map.intersection

-- | Does the map contain all these keys?
containsKeys :: Ord k => Map k v -> [k] -> Bool
containsKeys m = all (`Map.member` m)

-- | Convert a 2D List to a Map from coords to item
fromGrid :: [[a]] -> Map (Int, Int) a
fromGrid = Map.fromAscList
         . concat
         . zipWith (\x -> map $ first (x,)) [0..]
         . map (zip [0..])
