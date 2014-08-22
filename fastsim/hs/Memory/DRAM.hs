module Memory.DRAM where
import Data.Array
import Data.Bits
import Data.Maybe
import Control.Monad.Reader
import Access
import Model

data Bank = Bank {
    page :: Integer,
    dirty :: Bool,
    bankTime :: Double
}

emptyBank :: Bank
emptyBank = Bank {
    page = -1,
    dirty = False,
    bankTime = 0.0
}

data DRAMData = DRAMData {
    dramFrequency :: Double,
    casCycles :: Time,
    rcdCycles :: Time,
    rpCycles :: Time,
    wbCycles :: Time,
    pageSize :: Address,
    pageCount :: Address,
    width :: Address,
    burstSize :: Address,
    openPage :: Bool,
    ddr :: Bool,
    extraCycles :: Double,
    lastTime :: Time,
    writes :: Integer,
    banks :: Array Address Bank
}

emptyDRAM :: DRAMData
emptyDRAM = DRAMData {
    dramFrequency = 400000000.0,
    casCycles = 5,
    rcdCycles = 5,
    rpCycles = 5,
    wbCycles = 0,
    pageSize = 1024,
    pageCount = 65536,
    width = 8,
    burstSize = 4,
    openPage = True,
    ddr = True,
    extraCycles = 1.0,
    lastTime = 0,
    writes = 0,
    banks = array (0, 0) [(0, emptyBank)]
}

instance Memory DRAMData where
    set t "frequency" value = t { dramFrequency = (read value) }
    set t "cas_cycles" value = t { casCycles = (read value) }
    set t "rcd_cycles" value = t { rcdCycles = (read value) }
    set t "rp_cycles" value = t { rpCycles = (read value) }
    set t "wb_cycles" value = t { wbCycles = (read value) }
    set t "page_size" value = t { pageSize = (read value) }
    set t "page_count" value = t { pageCount = (read value) }
    set t "width" value = t { width = (read value) }
    set t "burst_size" value = t { burstSize = (read value) }
    set t "open_page" value = t { openPage = (read value) }
    set t "ddr" value = t { ddr = (read value) }
    set t "extra" value = t { extraCycles = (read value) }

    reset t model =
        let bs = (width t) * (burstSize t) in
        let bc = ((addressMask model) + bs) `div` bs in
        let bl = [(i, emptyBank) | i <- [0 .. bc - 1]] in
        t { banks = array (0, bc - 1) bl }

    wordSize t = (width t) * (burstSize t)

    writeCount t = (writes t)

    process t acc = do
        st <- lift simTime
        m <- asks model
        base <- asks baseAddress
        let addr = accessAddress acc
        let size = accessSize acc
        let realAddr = (addr + base) .&. (addressMask m)
        let start = max 0 $ (lastTime t) - st
        let wc = 1 + writes t
        return $ t { writes = wc }
