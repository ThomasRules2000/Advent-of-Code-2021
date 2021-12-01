module Util.NoQuotes where
import Control.DeepSeq

newtype NoQuotes = NoQuotes String
    deriving (Eq, Ord)

instance Show NoQuotes where 
    show (NoQuotes s) = s

instance NFData NoQuotes where
    rnf (NoQuotes s) = rnf s