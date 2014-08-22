module Memory.Cache where
import Data.Array
import Model

data CachePolicy = LRU | MRU | FIFO | PLRU

data CacheLine = CacheLine {
    tag :: Integer,
    age :: Integer,
    dirty :: Bool
}

data CacheData = CacheData {
    lineCount :: Int,
    lineSize :: Int,
    associativity :: Int,
    accessTime :: Int,
    cycleTime :: Int,
    policy :: CachePolicy,
    writeBack :: Bool,
    pending :: Integer,
    lines :: Array Integer Integer
}
