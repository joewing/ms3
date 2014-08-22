

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Memory.SPM where
import Access
import Model
import Control.Monad.Reader

data SPMData = SPMData {
    nextMemory :: forall m. Memory m => m,
    wordBytes :: Int,
    sizeBytes :: Int,
    accessTime :: Time,
    cycleTime :: Time,
    pending :: Time
}

instance Memory SPMData where
    set t "word_size" value = t { wordBytes = (read value) }
    set t "size" value = t { sizeBytes = (read value) }
    set t "access_time" value = t { accessTime = (read value) }
    set t "cycle_time" value = t { cycleTime = (read value) }

    reset t model = t {
        nextMemory = reset (nextMemory t) model,
        pending = 0
    }

    wordSize = wordBytes

    writeCount t = 0 --writeCount $ nextMemory t

    process t acc = do
        st <- lift simTime
        let addr = accessAddress acc
        let size = accessSize acc
        let result = max 0 ((pending t) - st)
        if addr < (sizeBytes t) then do
            let at = max ((cycleTime t) - (accessTime t)) 0
            let nextPending = st + at
            --advance (accessTime t)
            return $ t { pending = nextPending }
        else
            let nextPending = st + (pending t) in
            sendRequest (nextMemory t) acc
