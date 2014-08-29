
{-# LANGUAGE ExistentialQuantification #-}

module Model where
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.List(isPrefixOf)
import Data.Map
import Access

data Target = FPGA | ASIC

instance Show Target where
    show FPGA = "fpga"
    show ASIC = "asic"

instance Read Target where
    readsPrec _ str =
        case take 4 str of
            "fpga" -> [(FPGA, drop 4 str)]
            "asic" -> [(ASIC, drop 4 str)]
            _ -> []

data Goal = MinimizeTime | MinimizeWrites

instance Show Goal where
    show MinimizeTime = "time"
    show MinimizeWrites = "writes"

instance Read Goal where
    readsPrec _ str =
        if isPrefixOf "time" str then
            [(MinimizeTime, drop 4 str)]
        else if isPrefixOf "writes" str then
            [(MinimizeWrites, drop 6 str)]
        else []

data Model = Model {
    label :: String,
    target :: Target,
    optGoal :: Goal,
    fpgaPart :: String,
    frequency :: Double,
    maxCost :: Int,
    maxLuts :: Int,
    maxRegs :: Int,
    addressBits :: Address,
    addressMask :: Address,
    subsystems :: Map Int AnyMemory,
    baseAddresses :: Map Int Address,
    mainMemory :: Int,
    processes :: Map Int Int
}

instance Show Model where
    show m =
        "(label " ++ (label m) ++ ")\n" ++
        "(machine\n" ++
        "  (addr_bits " ++ (show (addressBits m)) ++ ")\n" ++
        "  (target " ++ (show (target m)) ++ ")\n" ++
        "  (part " ++ (fpgaPart m) ++ ")\n" ++
        ")\n"

data ProcessData = ProcessData {
    processId :: Int,
    simTime :: Time
}

type ProcessMonad a = StateT Model (State ProcessData) a

class Memory a where
    set :: a -> String -> String -> a
    reset :: a -> ProcessMonad a
    wordSize :: a -> ProcessMonad Address
    writeCount :: a -> ProcessMonad Integer
    process :: a -> Access -> ProcessMonad a

data AnyMemory = forall a. Memory a => MkMemory a

instance Memory AnyMemory where
    set (MkMemory m) k v = MkMemory $ set m k v
    reset (MkMemory m) = do
        updated <- reset m
        return $ MkMemory updated
    wordSize (MkMemory m) = do
        result <- wordSize m
        return result
    writeCount (MkMemory m) = do
        result <- writeCount m
        return $ result
    process (MkMemory m) a = do
        updated <- process m a
        return $ MkMemory updated

emptyModel :: Model
emptyModel = Model {
    label = "",
    target = FPGA,
    optGoal = MinimizeTime,
    fpgaPart = "xc6slx45",
    maxCost = 92,
    maxLuts = 10000,
    maxRegs = 40000,
    addressBits = 30,
    addressMask = 3,
    frequency = 100000000.0,
    subsystems = empty,
    baseAddresses = empty,
    processes = empty,
    mainMemory = 0
}

advance :: Integral a => a -> ProcessMonad ()
advance i = do
    pd <- lift get
    lift $ put $ pd { simTime = (simTime pd) + (fromIntegral i) }

baseAddress :: ProcessMonad Int
baseAddress = do
    pd <- lift get
    addrs <- gets baseAddresses
    return $ (addrs ! (processId pd))

getModel :: ProcessMonad Model
getModel = do
    st <- get
    return st

simulationTime :: ProcessMonad Time
simulationTime = do
    pd <- lift get
    return $ (simTime pd)

sendRequest :: Int -> Access -> ProcessMonad ()
sendRequest mid acc = do
    st <- get
    let mem = (subsystems st) ! mid
    let base = (baseAddresses st) ! mid
    ws <- wordSize mem
    let wmask = ws - 1
    let amask = addressMask st
    let offset = (accessAddress acc) .&. wmask
    let addrs = if offset == 0 then [0, ws .. (accessSize acc)]
            else offset : [ws, 2 * ws .. (accessSize acc) - (ws - offset)]
    let sizes = if offset == 0 then [ws, ws ..]
            else (ws - offset) : [ws, ws ..]
    let accesses = case acc of
            (Read _ _) -> [Read a s | (a, s) <- zip addrs sizes]
            (Write _ _) -> [Write a s | (a, s) <- zip addrs  sizes]
    updated <- foldM process mem accesses
    put $ st { subsystems = insert mid updated (subsystems st) }
