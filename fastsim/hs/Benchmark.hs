{-# LANGUAGE ExistentialQuantification #-}

module Benchmark where
import Access

data AnyBenchmark = forall a. Benchmark a => AnyBenchmark a

class Benchmark a where
    getIndex :: a -> Int
    isLast :: a -> Bool
    isIgnored :: a -> Bool
    set :: a -> String -> String -> a
    run :: a -> String -> IO [Access]
