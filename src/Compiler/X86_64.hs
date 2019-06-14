
module Compiler.X86_64 (compile) where

import Control.Monad.State  (runState, state, State(..))
import System.Process       (callCommand)
import Text.Printf          (printf)

--import Brainfuck        (Operation(..), Brainfuck)

--TODO import this from other file instead
data Operation
    = IncrementPointer
    | DecrementPointer
    | IncrementValue
    | DecrementValue
    | OutputValue
    | ReadValue
    | Loop [Operation]
    deriving (Show)

type Brainfuck = [Operation]

--data Instruction
--    = IncrementPointer
--    | DecrementPointer
--    | IncrementValue
--    | DecrementValue
--    | OutputValue
--    | ReadValue
--    | BeginLoop Int
--    | EndLoop Int

compile :: String -> Brainfuck -> IO ()
compile name bf = do
    let assembly = encode bf
    assemble name assembly

encode :: Brainfuck -> String
encode bf =
    let
        (asm, _)    = runState (encodeOperations bf) 0
    in
        header ++ asm ++ footer

encodeOperations :: Brainfuck -> State Int String
encodeOperations = fmap unlines . traverse encodeOperation

encodeOperation :: Operation -> State Int String
encodeOperation IncrementPointer    = return "incq %r14"
encodeOperation DecrementPointer    = return "decq %r14"
encodeOperation IncrementValue      = return "incb (%r15, %r14, 1)"
encodeOperation DecrementValue      = return "decb (%r15, %r14, 1)"
encodeOperation OutputValue         = return "call _printChar"
encodeOperation ReadValue           = undefined --TODO implement
encodeOperation (Loop bf)           = state $ \lc ->
    let
        loopStart       = printf "l" ++ show lc ++ "_start:\ncmpb $0, (%r15, %r14, 1)\nje l" ++ show lc ++ "_end\n\n"
        loopEnd         = printf "jmp l" ++ show lc ++ "_start\nl" ++ show lc ++ "_end:\n"
        (loopBody, lc') = runState (encodeOperations bf) (lc + 1)
    in
        (loopStart ++ loopBody ++ loopEnd, lc')

assemble :: String -> String -> IO ()
assemble programName assemblyCode = do
    callCommand $ "echo '" ++ assemblyCode ++ "' > hello.s" --TODO remove
    callCommand $ "echo '" ++ assemblyCode ++ "' | as -o " ++ programName ++ ".o"
    callCommand $ "ld " ++ programName ++ ".o -o " ++ programName

-- TODO call from Main file
main :: IO ()
main = do
    assemble "hello" (encode [IncrementValue, Loop [DecrementValue], Loop [Loop [DecrementValue]]])

hello = unlines
    [".section .data",
    ".section .text",
    "message:",
    ".ascii \"Hello world\\\\n\"",
    ".equ message_length, 12",
    ".globl _start",
    "_start:",
    "movq $1, %rdi       # stdout file descriptor",
    "movq $message, %rsi # message to print",
    "movq $message_length, %rdx      # message length",
    "movq $1, %rax       # sys_write",
    "syscall",
    "movq $0, %rdi       # exit code = 0",
    "movq $60, %rax      # sys_exit",
    "syscall"]

header = "\n\
    \.section .bss\n\
    \    .lcomm memory, 30000\n\
    \\\n\
    \.section .text\n\
    \    .global _start\n\
    \\\n\
    \_printChar:\n\
    \movq $1, %rdi       # stdout file descriptor\n\
    \movq $memory, %rsi  # message to print\n\
    \addq %r14, %rsi     # TODO can this be done in one step?\n\
    \movq $1, %rdx       # message length\n\
    \movq $1, %rax       # sys_write\n\
    \syscall\n\
    \ret\n\
    \\\n\
    \_start:\n\
    \movq $memory, %r15\n\
    \movq $0, %r14       # index\n\n"

footer = "\n\
    \movq $0, %rdi       # exit code = 0\n\
    \movq $60, %rax      # sys_exit\n\
    \syscall\n"

