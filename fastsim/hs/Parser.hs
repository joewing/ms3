module Parser where
import Memory
import Text.ParserCombinators.Parsec

type ModelParser a = GenParser Char a

parseFile :: ModelParser Model
parseFile = parseModel emptyModel

parseModel :: Model -> ModelParser Model
parseModel m = do
    name <- parseStart
    case name of
        "label" -> parseLabel m
        "machine" -> parseMachine m
        "memory" -> parseMemoryList m
        "benchmarks" -> parseBenchmarks m

spaces :: ModelParser Char
spaces = many (char " \n\t")

parseStart :: ModelParser String
parseStart = do
    spaces
    char '('
    spaces
    result <- many (noneOf " )")
    spaces
    return result

parseEnd :: ModelParser Char
parseEnd = spaces >> char ')'

parseLabel :: ModelModel -> GenParser Char st Model
parseLabel m = do
    label <- many (noneOf ")")
    return $ m { label = label }

processMachineArgs :: Model -> [(String, String)] -> ModelParser Model

parseMachine :: Model -> ModelParser Model
parseMachine m = do
    args <- parseArguments
    parseEnd
    processMachineArgs m args

parseArguments :: ModelParser [(String, String)]
parseArguments = many parseArgument

parseArgument :: ModelParser (String, String)
parseArgument = do
    char '('
    key <- many (noneOf " ")
    value <- many (noneOf ")")
    char ')'
    return (key, value)
