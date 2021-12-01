{-# LANGUAGE ScopedTypeVariables #-}
module Program.RunDay where
import           Control.Exception
import           Data.Functor
import           System.CPUTime    (getCPUTime)
import           Text.Printf
import Control.DeepSeq

runDay :: (Show out1, Show out2, NFData out1, NFData out2, NFData inp) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO (Maybe Integer, Maybe Integer)
runDay parser part1 part2 s = do
    file <- try $ parser <$> readFile s
    case file of
        Left (e :: SomeException) -> print e >> return (Nothing, Nothing)
        Right input -> do          
            parserStart <- getCPUTime
            parserEnd <- input `deepseq` getCPUTime
            let parserTime = (parserEnd - parserStart)
            putStrLn $ "Parser " ++ timeString parserTime
            
            p1Start <- getCPUTime
            p1Res <- catch (return $ Just (part1 input)) $ \(e :: SomeException) -> print e >> return Nothing
            p1End <- p1Res `deepseq` getCPUTime

            let p1Time = (p1End - p1Start)
            putStrLn $ "Part 1 " ++ timeString p1Time ++ ":"
            putStrLn $ maybe "Unable to run Part 1!" show p1Res

            p2Start <- getCPUTime
            p2Res <- catch (return $ Just (part2 input)) $ \(e :: SomeException) -> print e >> return Nothing
            p2End <- p2Res `deepseq` getCPUTime

            let p2Time = (p2End - p2Start)
            putStrLn $ printf "Part 2 " ++ timeString p2Time ++ ":"
            putStrLn $ maybe "Unable to run Part 2!" show p2Res

            return (p1Res $> p1Time, p2Res $> p2Time)


timeString :: Integer -> String
timeString t
    | t > 10^11 = printf "(%.2fs)"  $ (fromIntegral t / (10^12) :: Double)
    | t > 10^8  = printf "(%.2fms)" $ (fromIntegral t / (10^9)  :: Double)
    | t > 10^5  = printf "(%.2fμs)" $ (fromIntegral t / (10^6)  :: Double)
    | t > 100   = printf "(%.2fns)" $ (fromIntegral t / (10^3)  :: Double)
    | otherwise = printf "(%dps)" t