module Util.Set where
-- A couple of useful digraphs for union and intersection
import           Data.Set (Set, intersection, union)

(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = union

(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = intersection
