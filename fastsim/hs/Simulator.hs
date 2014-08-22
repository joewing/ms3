module Simulator where
import Memory
import Subsystem

data Process = Process {
    mem :: Subsystem
}

data Simulator = Simulator {
    model :: Model,
    directory :: String,
    processes :: [Process]
}
