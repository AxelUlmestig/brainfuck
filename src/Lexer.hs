
module Lexer (Brainfuck(..), pBrainfuck) where

import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

data Brainfuck
    = IncrementPointer
    | DecrementPointer
    | IncrementValue
    | DecrementValue
    | OutputValue
    | ReadValue
    | Loop [Brainfuck]
    deriving (Show)

pBrainfuck :: Parser [Brainfuck]
pBrainfuck = catMaybes <$> (many $ choice [
        pIncrementPointer,
        pDecrementPointer,
        pIncrementValue,
        pDecrementValue,
        pOutputValue,
        pReadValue,
        pLoop,
        pIgnored
    ])

pIncrementPointer :: Parser (Maybe Brainfuck)
pIncrementPointer = parseChar '>' IncrementPointer

pDecrementPointer :: Parser (Maybe Brainfuck)
pDecrementPointer = parseChar '<' DecrementPointer

pIncrementValue :: Parser (Maybe Brainfuck)
pIncrementValue = parseChar '+' IncrementValue

pDecrementValue :: Parser (Maybe Brainfuck)
pDecrementValue = parseChar '-' DecrementValue

pOutputValue :: Parser (Maybe Brainfuck)
pOutputValue = parseChar '.' OutputValue

pReadValue :: Parser (Maybe Brainfuck)
pReadValue = parseChar ',' ReadValue

pLoop :: Parser (Maybe Brainfuck)
pLoop = Just . Loop <$> between (char '[') (char ']') pBrainfuck

pIgnored :: Parser (Maybe Brainfuck)
pIgnored = many1 (noneOf "><+-.,[]") *> pure Nothing

parseChar :: Char -> Brainfuck -> Parser (Maybe Brainfuck)
parseChar ch bf = const (Just bf) <$> char ch

