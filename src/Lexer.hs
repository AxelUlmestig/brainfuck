
module Lexer (pBrainfuck) where

import Data.Functor                   (
    ($>)
  )
import Data.Maybe                     (
    catMaybes
  )
import Text.Parsec                    (
    getState,
    modifyState,
    Parsec
  )
import Text.ParserCombinators.Parsec  (
    between,
    char,
    choice,
    many,
    many1,
    noneOf
  )

import Brainfuck                      (
    Operation(
      IncrementPointer,
      IncrementValue,
      Loop,
      OutputValue,
      ReadValue
    ),
    Brainfuck
  )

pBrainfuck :: Parsec String Int Brainfuck
pBrainfuck = catMaybes <$> many (choice [
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
pIncrementPointer = parseChar '>' (IncrementPointer 1)

pDecrementPointer :: Parsec String Int (Maybe Operation)
pDecrementPointer = parseChar '<' (IncrementPointer (-1))

pIncrementValue :: Parsec String Int (Maybe Operation)
pIncrementValue = parseChar '+' (IncrementValue 1)

pDecrementValue :: Parsec String Int (Maybe Operation)
pDecrementValue = parseChar '-' (IncrementValue (-1))

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
pIgnored = many1 (noneOf "><+-.,[]") $> Nothing

parseChar :: Char -> Operation -> Parsec String Int (Maybe Operation)
parseChar ch bf = const (Just bf) <$> char ch

