{-# LANGUAGE ScopedTypeVariables #-}
module Program.RunDay where
import           Control.Exception
import           Data.Functor
import           System.CPUTime    (getCPUTime)
import           Text.Printf
import Control.DeepSeq

runDay :: (Show out1, Show out2, NFData out1, NFData out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO (Maybe Double, Maybe Double)
runDay parser part1 part2 s = do
    file <- try $ parser <$> readFile s
    case file of
        Left (e :: SomeException) -> print e >> return (Nothing, Nothing)
        Right input -> do
            p1Start <- getCPUTime
            p1Res <- catch (return $ Just (part1 input)) $ \(e :: SomeException) -> print e >> return Nothing
            p1End <- p1Res `deepseq` getCPUTime

            let p1Time = fromIntegral (p1End - p1Start) / (10^12) :: Double
            putStrLn $ printf "Part 1 (%.2fs):" p1Time
            putStrLn $ maybe "Unable to run Part 1!" show p1Res

            p2Start <- getCPUTime
            p2Res <- catch (return $ Just (part2 input)) $ \(e :: SomeException) -> print e >> return Nothing
            p2End <- p2Res `deepseq` getCPUTime

            let p2Time = fromIntegral (p2End - p2Start) / (10^12) :: Double
            putStrLn $ printf "Part 2 (%.2fs):" p2Time
            putStrLn $ maybe "Unable to run Part 2!" show p2Res

            return (p1Res $> p1Time, p2Res $> p2Time)
