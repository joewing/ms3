module Parser(parseFile)  where
import Data.Bits
import Text.ParserCombinators.Parsec
import System.IO
import Memory.DRAM
import Model

parseFile :: String -> IO Model
parseFile fileName = do
    h <- openFile fileName ReadMode
    contents <- hGetContents h
    case parseTop fileName contents of
        Left msg -> fail $ show msg
        Right model -> return model

parseTop :: String -> String -> Either ParseError Model
parseTop = parse (parseModel emptyModel)

parseModel m = do
    name <- parseStart
    case name of
        "label" -> parseLabel m >>= parseModel
        "machine" -> parseMachine m >>= parseModel
        "memory" -> parseMemoryList m >>= parseModel
        "benchmarks" -> parseBenchmarks m >>= parseModel
        _ -> fail $ "unexpected section: " ++ name

parseLabel :: Model -> GenParser Char st Model
parseLabel m = do
    str <- many (noneOf ")")
    parseEnd
    return $ m { Model.label = str }

parseMachine m = do
    args <- parseArguments
    parseEnd
    processMachineArgs m args

parseMemoryList m =
        (parseMainMemory m >>= parseMemoryList)
    <|> (parseSubsystem m >>= parseMemoryList)
    <|> (parseFIFO m >>= parseMemoryList)
    <|> (parseEnd >> return m)

parseBenchmarks m =
        (parseTrace m >>= parseBenchmarks)
    <|> (parseEnd >> return m)

processMachineArgs m ((key, value):args) =
    case key of
        "addr_bits" ->
            let bits = read value in
            let mask = (1 `shiftL` bits) - 1 in
            let updated = m { addressBits = bits, addressMask = mask } in
            processMachineArgs updated args
        "target" -> processMachineArgs (m { target = (read value) }) args
        "part" -> processMachineArgs (m { fpgaPart = (read value) }) args
        "frequency" -> processMachineArgs (m { frequency = (read value) }) args
        "max_cost" -> processMachineArgs (m { maxCost = (read value) }) args
        "max_luts" -> processMachineArgs (m { maxLuts = (read value) }) args
        "max_regs" -> processMachineArgs (m { maxRegs = (read value) }) args
        "goal" -> processMachineArgs (m { optGoal = (read value) }) args
        _ -> fail $ "invalid machine argument: " ++ key
processMachineArgs m [] = return m

parseMainMemory m = do
    parseNamedStart "main"
    parseNamedStart "memory"
    result <- parseMemory
    parseEnd
    parseEnd
    return $ m { mainMemory = 0 }

parseSubsystem m = do
    parseNamedStart "subsystem"
    parseEnd
    return m

parseFIFO m = do
    parseNamedStart "fifo"
    parseEnd
    return m

parseMemory = parseDRAM 

parseDRAM = do
    parseNamedStart "dram"
    args <- parseArguments
    return $ emptyDRAM

parseTrace m = do
    parseNamedStart "trace"
    parseEnd
    return m

parseArguments = many parseArgument

parseArgument = do
    spaces
    char '('
    key <- many (noneOf " \n\t")
    spaces
    value <- many (noneOf ")")
    char ')'
    spaces
    return (key, value)

parseNamedStart name = do
    spaces
    char '('
    string name
    spaces
    return ()

parseStart = do
    spaces
    char '('
    result <- many (noneOf " \n\t)")
    spaces
    return result

parseEnd = spaces >> char ')'
