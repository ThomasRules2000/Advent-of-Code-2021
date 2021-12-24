module Util.Map where
import           Data.Bifunctor  (first, second)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Tuple      (swap)

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

-- | Invert a map to give a map from values to sets of keys
invert :: (Ord k, Ord v) => Map k v -> Map v (Set k)
invert = Map.fromListWith Set.union 
       . map (second Set.singleton . swap) 
       . Map.toList
