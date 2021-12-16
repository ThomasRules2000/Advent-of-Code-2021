module Days.Day16 where
import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (bimap, first)
import           Data.List       (uncons)
import           GHC.Generics    (Generic)
import qualified Program.RunDay  as R (runDay)
import           Util.Util       (binToDec, hexToBin)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = Packet

type Output1 = Int
type Output2 = Int

data Packet = Literal Int Int
            | Operator Int PacketType [Packet]
            deriving (Eq, Ord, Show, Generic, NFData)

data PacketType = Sum
                | Product
                | Minimum
                | Maximum
                | Lit -- Should never be used but useful for enum
                | GreaterThan
                | LessThan
                | EqualTo
                deriving (Eq, Ord, Show, Enum, Generic, NFData)

parser :: String -> Input
parser = fst . readPacket . hexToBin

readPacket :: [Bool] -> (Packet, [Bool])
readPacket packet
    | ty == 4 = first (Literal version) $ readLiteral rest
    | lengthType = (Operator version packetType lt1Ps, lt1Rest)
    | otherwise  = (Operator version packetType lt0Ps, lt0Rest)
    where
        (version, (ty, rest)) = bimap binToDec (first binToDec . splitAt 3) $ splitAt 3 packet
        Just (lengthType, ltRest) = uncons rest
        (lt0Ps, lt0Rest) = first lengthTypeZero $ uncurry splitAt $ first binToDec $ splitAt 15 ltRest
        (lt1Ps, lt1Rest) = uncurry lengthTypeOne $ first binToDec $ splitAt 11 ltRest
        packetType = toEnum ty

lengthTypeOne :: Int -> [Bool] -> ([Packet], [Bool])
lengthTypeOne 0 bs = ([], bs)
lengthTypeOne n bs = first (p:) $ lengthTypeOne (n-1) newBs
    where (p, newBs) = readPacket bs

lengthTypeZero :: [Bool] -> [Packet]
lengthTypeZero bs
    | null newBs = [p]
    | otherwise = p : lengthTypeZero newBs
    where (p, newBs) = readPacket bs

readLiteral :: [Bool] -> (Int, [Bool])
readLiteral = first binToDec . go
    where
        go :: [Bool] -> ([Bool], [Bool])
        go (cont:rest)
            | cont = first (num++) $ go next
            | otherwise = (num, next)
            where (num, next) = splitAt 4 rest
        go [] = ([], [])


part1 :: Input -> Output1
part1 = sumPacketVersions

sumPacketVersions :: Packet -> Int
sumPacketVersions (Literal v _)     = v
sumPacketVersions (Operator v _ ps) = (v +) $ sum $ map sumPacketVersions ps


part2 :: Input -> Output2
part2 = evalPacket

evalPacket :: Packet -> Int
evalPacket (Literal _ n) = n
evalPacket (Operator _ ty ps) = case ty of
    Sum         -> sum     evaled
    Product     -> product evaled
    Minimum     -> minimum evaled
    Maximum     -> maximum evaled
    GreaterThan -> fromEnum $ x > y
    LessThan    -> fromEnum $ x < y
    EqualTo     -> fromEnum $ x == y
    Lit         -> error "Invalid op type"
    where
        evaled = map evalPacket ps
        [x,y] = evaled

