
module Lexer (pBrainfuck) where

import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

import Brainfuck (Operation(..), Brainfuck)

pBrainfuck :: Parser Brainfuck
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

pIncrementPointer :: Parser (Maybe Operation)
pIncrementPointer = parseChar '>' IncrementPointer

pDecrementPointer :: Parser (Maybe Operation)
pDecrementPointer = parseChar '<' DecrementPointer

pIncrementValue :: Parser (Maybe Operation)
pIncrementValue = parseChar '+' IncrementValue

pDecrementValue :: Parser (Maybe Operation)
pDecrementValue = parseChar '-' DecrementValue

pOutputValue :: Parser (Maybe Operation)
pOutputValue = parseChar '.' OutputValue

pReadValue :: Parser (Maybe Operation)
pReadValue = parseChar ',' ReadValue

pLoop :: Parser (Maybe Operation)
pLoop = Just . Loop <$> between (char '[') (char ']') pBrainfuck

pIgnored :: Parser (Maybe Operation)
pIgnored = many1 (noneOf "><+-.,[]") *> pure Nothing

parseChar :: Char -> Operation -> Parser (Maybe Operation)
parseChar ch bf = const (Just bf) <$> char ch

