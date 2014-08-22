
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Model where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Access
import Benchmark

data Model = Model {
    label :: String,
    addressBits :: Address,
    addressMask :: Address,
    frequency :: Double,
    mainMemory :: forall m. Memory m => Maybe m,
    subsystems :: forall m. Memory m => [m],
    fifos :: forall m. Memory m => [m],
    benchmarks :: [AnyBenchmark]
}

data MemoryState = MemoryState {
    model :: Model,
    baseAddress :: Address
}

data SimTime a = SimTime (Time -> (a, Time))

type ProcessMonad a = ReaderT MemoryState SimTime a

class Memory a where
    set :: a -> String -> String -> a
    reset :: a -> Model -> a
    wordSize :: a -> Address
    writeCount :: a -> Integer
    process :: a -> Access -> ProcessMonad a

instance Monad SimTime where
    return x = SimTime $ \t -> (x, t)
    (SimTime f) >>= g = SimTime $ \t ->
        let (x, t') = f t in
        let (SimTime h) = g x in
        h t'

advance :: Integral a => a -> SimTime ()
advance i = SimTime $ \t -> ((), t + fromIntegral i)

simTime :: SimTime Time
simTime = SimTime (\t -> (t, t))

emptyModel :: Model
emptyModel = Model {
    label = "",
    addressBits = 30,
    addressMask = 3,
    frequency = 100000000.0,
    mainMemory = Nothing,
    subsystems = [],
    fifos = [],
    benchmarks = []
}

sendRequest :: Memory a => a -> Access -> ProcessMonad a
sendRequest mem acc = do
    model <- asks model
    base <- asks baseAddress
    let ws = wordSize mem
    let wmask = ws - 1
    let amask = addressMask model
    let offset = (accessAddress acc) .&. wmask
    let addrs = if offset == 0 then [0, ws .. (accessSize acc)]
            else offset : [ws, 2 * ws .. (accessSize acc) - (ws - offset)]
    let sizes = if offset == 0 then [ws, ws ..]
            else (ws - offset) : [ws, ws ..]
    let accesses = case acc of
            (Read _ _) -> [Read a s | (a, s) <- zip addrs sizes]
            (Write _ _) -> [Write a s | (a, s) <- zip addrs  sizes]
    foldM process mem accesses
