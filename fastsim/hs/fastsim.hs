module Main where

import Control.Monad.Error
import System.Environment
import System.Exit

import Benchmark
import Benchmark.Trace

parseArgument :: String -> String -> [String] -> String
parseArgument name def (x:xs) =
    if x == name then
        case xs of
            (value:_) -> value
            _ -> error "invalid argument"
    else parseArgument name def xs
parseArgument name def [] = def

parseIntArgument :: String -> Integer -> [String] -> Integer
parseIntArgument name def args = read (parseArgument name (show def) args)

main :: IO ()
main = do
    args <- getArgs
    let subsystem = parseIntArgument "-s" (-1) args
    let directory = parseArgument "-d" "/" args
    let bm = TraceBenchmark {
        index = 1,
        final = True,
        ignored = False,
        name = "bah"
    }
    results <- run bm directory
    putStrLn $ show results
