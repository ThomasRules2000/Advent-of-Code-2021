module Main where
import           Control.Monad
import           Data.List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           System.Environment
import           Text.Printf
import           Text.Read

{- ORMOLU_DISABLE -}
import qualified Days.Day01         as Day01 (runDay)
import qualified Days.Day02         as Day02 (runDay)
import qualified Days.Day03         as Day03 (runDay)
import qualified Days.Day04         as Day04 (runDay)
import qualified Days.Day05         as Day05 (runDay)
import qualified Days.Day06         as Day06 (runDay)
import qualified Days.Day07         as Day07 (runDay)
import qualified Days.Day08         as Day08 (runDay)
import qualified Days.Day09         as Day09 (runDay)
import qualified Days.Day10         as Day10 (runDay)
import qualified Days.Day11         as Day11 (runDay)
import qualified Days.Day12         as Day12 (runDay)
import qualified Days.Day13         as Day13 (runDay)
import qualified Days.Day14         as Day14 (runDay)
import qualified Days.Day15         as Day15 (runDay)
import qualified Days.Day16         as Day16 (runDay)
import qualified Days.Day17         as Day17 (runDay)
import qualified Days.Day18         as Day18 (runDay)
import qualified Days.Day19         as Day19 (runDay)
import qualified Days.Day20         as Day20 (runDay)
import qualified Days.Day21         as Day21 (runDay)
import qualified Days.Day22         as Day22 (runDay)
import qualified Days.Day23         as Day23 (runDay)
import qualified Days.Day24         as Day24 (runDay)
import qualified Days.Day25         as Day25 (runDay)
{- ORMOLU_ENABLE -}

days :: [(String -> IO (Maybe Double, Maybe Double), String)]
days = [
    (Day01.runDay, "input/Day01.txt"),
    (Day02.runDay, "input/Day02.txt"),
    (Day03.runDay, "input/Day03.txt"),
    (Day04.runDay, "input/Day04.txt"),
    (Day05.runDay, "input/Day05.txt"),
    (Day06.runDay, "input/Day06.txt"),
    (Day07.runDay, "input/Day07.txt"),
    (Day08.runDay, "input/Day08.txt"),
    (Day09.runDay, "input/Day09.txt"),
    (Day10.runDay, "input/Day10.txt"),
    (Day11.runDay, "input/Day11.txt"),
    (Day12.runDay, "input/Day12.txt"),
    (Day13.runDay, "input/Day13.txt"),
    (Day14.runDay, "input/Day14.txt"),
    (Day15.runDay, "input/Day15.txt"),
    (Day16.runDay, "input/Day16.txt"),
    (Day17.runDay, "input/Day17.txt"),
    (Day18.runDay, "input/Day18.txt"),
    (Day19.runDay, "input/Day19.txt"),
    (Day20.runDay, "input/Day20.txt"),
    (Day21.runDay, "input/Day21.txt"),
    (Day22.runDay, "input/Day22.txt"),
    (Day23.runDay, "input/Day23.txt"),
    (Day24.runDay, "input/Day24.txt"),
    (Day25.runDay, "input/Day25.txt")
    ]

main :: IO ()
main = do
    args <- getArgs
    let toRun = if "-a" `elem` args then [1..25] else mapMaybe readMaybe args
    times <- Map.fromList . zip toRun <$> mapM (\arg -> uncurry (performDay arg) $ fromMaybe (error $ printf "Day %d not found" arg) $ lookup arg (zip [1..] days)) toRun
    printSummary times

performDay :: Int -> (String -> IO (Maybe Double, Maybe Double)) -> String -> IO (Maybe Double, Maybe Double)
performDay day func file = do
    putStrLn $ printf "===== DAY %d =====" day
    func file

printSummary :: Map Int (Maybe Double, Maybe Double) -> IO ()
printSummary results = do
    putStrLn "====== SUMMARY ======"
    let part1s = Map.mapKeys ((++".1") . show) $ fst <$> results
        part2s = Map.mapKeys ((++".2") . show) $ snd <$> results
        parts = Map.toList $ part1s <> part2s

        fails = [p | (p, Nothing) <- parts]
        fasts = [(p, t) | (p, Just t) <- parts, t < 1]
        slows = [(p, t) | (p, Just t) <- parts, t >= 1]

        greatSuccess = null fails && null slows
        nFails = if null fails then "No" else show $ length fails
        nFasts = if null fasts then "No" else show $ length fasts
        nSlows = if null slows then "No" else show $ length slows

    when greatSuccess $ putStr "All "
    putStr $ printf "%s parts completed under 1 second" nFasts
    unless greatSuccess $ do
        putStrLn "\n Of the remainder:"
        unless (null fails) $ do
            putStrLn $ printf "  %s parts failed" nFails
            putStrLn $ "    " ++ intercalate ", " fails
        unless (null slows) $ do
            putStrLn $ printf "  %s parts took longer than 1s" nSlows
            putStrLn $ "    " ++ intercalate ", " (map (uncurry (printf "%s (%.2fs)")) slows)
