module Util.Set where
import           Data.Maybe (fromJust, isJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set

-- | Infix `union`
(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = Set.union

-- | Infix `intersection`
(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection

-- | Take a `Set` of `Maybe`s and return a `Set` of all the `Just` values
catMaybes :: Ord a => Set (Maybe a) -> Set a
catMaybes = Set.map fromJust . Set.filter isJust
