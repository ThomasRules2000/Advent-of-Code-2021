{-# LANGUAGE ScopedTypeVariables #-}
module Program.RunDay where
import           Control.Exception (SomeException, evaluate, try)
import           Data.Either.Extra (eitherToMaybe)
import           Data.Functor      (($>))
import           Formatting        (formatToString)
import           Formatting.Clock  (timeSpecs)
import           System.Clock      (Clock (Monotonic), TimeSpec, getTime)
import           Text.Printf       (printf)

runDay :: (Show out1, Show out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay parser part1 part2 s = do
    parserStart <- getTime Monotonic
    file <- try $ readFile s >>= (evaluate . parser)
    parserEnd <- getTime Monotonic
    case file of
        Left (e :: SomeException) -> putStrLn "Unable to parse input!" >> print e $> (Nothing, Nothing, Nothing)
        Right input -> do
            let parserTime = parserEnd - parserStart
            putStrLn $ "Parser (" ++ formatToString timeSpecs parserStart parserEnd ++ ")"

            p1Start <- getTime Monotonic
            p1Res <- try $ evaluate $ part1 input
            p1End <- getTime Monotonic

            let p1Time = p1End - p1Start
            putStrLn $ "Part 1 (" ++ formatToString timeSpecs p1Start p1End ++ "):"
            putStrLn $ either (\(e :: SomeException) -> "Unable to run Part 1!\n" ++ show e) show p1Res

            p2Start <- getTime Monotonic
            p2Res <- try (evaluate (part2 input))
            p2End <- getTime Monotonic

            let p2Time = p2End - p2Start
            putStrLn $ printf "Part 2 (" ++ formatToString timeSpecs p2Start p2End ++ "):"
            putStrLn $ either (\(e :: SomeException) -> "Unable to run Part 2!\n" ++ show e) show p2Res

            putStrLn $ "Total Time: " ++ formatToString timeSpecs 0 (parserTime + p1Time + p2Time)

            return (Just parserTime, eitherToMaybe p1Res $> p1Time, eitherToMaybe p2Res $> p2Time)
