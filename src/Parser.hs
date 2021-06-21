module Parser (pBrainfuck) where

import           Data.Functor                  (($>))
import           Data.Maybe                    (catMaybes)
import           Data.Text                     (Text)
import           Text.Parsec                   (Parsec, getState, modifyState)
import           Text.ParserCombinators.Parsec (between, char, choice, many,
                                                many1, noneOf)

import           Brainfuck                     (Brainfuck,
                                                Operation (IncrementPointer, IncrementValue, Loop, OutputValue, ReadValue))

pBrainfuck :: Parsec Text Int Brainfuck
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

pIncrementPointer :: Parsec Text Int (Maybe Operation)
pIncrementPointer = parseChar '>' (IncrementPointer 1)

pDecrementPointer :: Parsec Text Int (Maybe Operation)
pDecrementPointer = parseChar '<' (IncrementPointer (-1))

pIncrementValue :: Parsec Text Int (Maybe Operation)
pIncrementValue = parseChar '+' (IncrementValue 1)

pDecrementValue :: Parsec Text Int (Maybe Operation)
pDecrementValue = parseChar '-' (IncrementValue (-1))

pOutputValue :: Parsec Text Int (Maybe Operation)
pOutputValue = parseChar '.' OutputValue

pReadValue :: Parsec Text Int (Maybe Operation)
pReadValue = parseChar ',' ReadValue

pLoop :: Parsec Text Int (Maybe Operation)
pLoop = do
    loop        <- between (char '[') (char ']') pBrainfuck
    loopCount   <- getState
    modifyState (+1)
    return $ Just (Loop loopCount loop)

pIgnored :: Parsec Text Int (Maybe Operation)
pIgnored = many1 (noneOf "><+-.,[]") $> Nothing

parseChar :: Char -> Operation -> Parsec Text Int (Maybe Operation)
parseChar ch bf = const (Just bf) <$> char ch

