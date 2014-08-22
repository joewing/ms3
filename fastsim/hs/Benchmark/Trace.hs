module Benchmark.Trace where
import Data.Char
import System.IO
import Benchmark

data TraceBenchmark = TraceBenchmark {
    index :: Int,
    final :: Bool,
    ignored :: Bool,
    name :: String
}

parseTrace :: String -> [Access]
parseTrace str = do
    (acc, left) <- reads str
    acc : parseTrace (skipChar left)

instance Benchmark TraceBenchmark where

    getIndex t = index t
    isLast t = final t
    isIgnored t = ignored t

    set t name value =
        case name of
            "index" -> t { index = (read value) }
            "last" -> t { final = value == "true" }
            "ignored" -> t { ignored = value == "true" }
            _ -> error "invalid parameter for trace"

    run t dir = do
        let full = dir ++ "/" ++ (name t) ++ ".trace"
        h <- openFile full ReadMode
        contents <- hGetContents h
        let parsed = parseTrace contents
        if (final t) then
            return parsed
        else
            let result = parsed ++ result in
            return result
