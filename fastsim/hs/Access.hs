module Access where
import Numeric

type Address = Int

type Time = Integer

type Channel = Int

data Access = Read Address Int
            | Write Address Int
            | Modify Address Int
            | Idle Time Int
            | Produce Channel Int
            | Consume Channel Int
            | Terminate
            | Invalid

instance Show Access where
    show (Read a s) = showAccess 'R' a s
    show (Write a s) = showAccess 'W' a s
    show (Modify a s) = showAccess 'M' a s
    show (Idle a s) = showAccess 'I' a s
    show (Produce a s) = showAccess 'P' a s
    show (Consume a s) = showAccess 'C' a s
    show Terminate = "X0:0"
    show Invalid = "?"

instance Read Access where
    readsPrec _ (x:xs) = do
        (addr, ys) <- readHex xs
        (size, zs) <- readHex $ skipChar ys
        return $ (parseAccess x addr size, zs)
    readsPrec _ [] = []

skipChar :: String -> String
skipChar (x:xs) = xs
skipChar [] = []

parseAccess :: Integral a => Char -> a -> Int -> Access
parseAccess ch addr size =
    case ch of
        'R' -> Read (fromIntegral addr) size
        'W' -> Write (fromIntegral addr) size
        'M' -> Modify (fromIntegral addr) size
        'I' -> Idle (fromIntegral addr) size
        'P' -> Produce (fromIntegral addr) size
        'C' -> Consume (fromIntegral addr) size
        'X' -> Terminate
        _   -> Invalid

showAccess :: (Integral a, Show a) => Char -> a -> Int -> String
showAccess ch addr size = ch : (showHex addr (':' : (showHex size "")))

accessAddress :: Access -> Address
accessAddress (Read addr _) = addr
accessAddress (Write addr _) = addr
accessAddress (Modify addr _) = addr

accessSize :: Access -> Int 
accessSize (Read _ size) = size
accessSize (Write _ size) = size
accessSize (Modify _ size) = size

isWrite :: Access -> Bool
isWrite (Write _ _) = True
isWrite _ = False
