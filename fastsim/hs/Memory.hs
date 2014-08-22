module Memory where
import Control.Monad.Reader
import Memory.DRAM
import Memory.SPM
import Memory.Cache
import Model

data AnyMemory = DRAM DRAMData
               | SPM SPMData

instance Memory AnyMemory where

    set (DRAM t) k v = DRAM $ set t k v
    set (SPM t) k v = SPM $ set t k v

    reset (DRAM t) m = DRAM $ reset t m
    reset (SPM t) m = SPM $ reset t m

    wordSize (DRAM t) = wordSize t
    wordSize (SPM t) = wordSize t

    writeCount (DRAM t) = writeCount t
    writeCount (SPM t) = writeCount t

    process (DRAM t) a = process t a >>= (\r -> return $ DRAM r)
    process (SPM t) a = process t a >>= (\r -> return $ SPM r)

