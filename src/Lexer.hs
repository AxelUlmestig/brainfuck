
module Lexer (pBrainfuck) where

import Data.Maybe   (catMaybes)
import Text.Parsec  (getState, modifyState, Parsec)
import Text.ParserCombinators.Parsec

import Brainfuck    (Operation(..), Brainfuck)

pBrainfuck :: Parsec String Int Brainfuck
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

pIncrementPointer :: Parsec String Int (Maybe Operation)
pIncrementPointer = parseChar '>' IncrementPointer

pDecrementPointer :: Parsec String Int (Maybe Operation)
pDecrementPointer = parseChar '<' DecrementPointer

pIncrementValue :: Parsec String Int (Maybe Operation)
pIncrementValue = parseChar '+' IncrementValue

pDecrementValue :: Parsec String Int (Maybe Operation)
pDecrementValue = parseChar '-' DecrementValue

pOutputValue :: Parsec String Int (Maybe Operation)
pOutputValue = parseChar '.' OutputValue

pReadValue :: Parsec String Int (Maybe Operation)
pReadValue = parseChar ',' ReadValue

pLoop :: Parsec String Int (Maybe Operation)
pLoop = do
    loop        <- between (char '[') (char ']') pBrainfuck
    loopCount   <- getState
    modifyState (+1)
    return $ Just (Loop loopCount loop)

pIgnored :: Parsec String Int (Maybe Operation)
pIgnored = many1 (noneOf "><+-.,[]") *> pure Nothing

parseChar :: Char -> Operation -> Parsec String Int (Maybe Operation)
parseChar ch bf = const (Just bf) <$> char ch

