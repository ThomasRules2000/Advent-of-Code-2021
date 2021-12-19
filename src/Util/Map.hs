module Util.Map where
import Data.Map.Strict

(\/) :: Ord k => Map k v -> Map k v -> Map k v
(\/) = union

(/\) :: Ord k => Map k v -> Map k v -> Map k v
(/\) = intersection