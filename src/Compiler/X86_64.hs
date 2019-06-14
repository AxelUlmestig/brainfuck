
module Compiler.X86_64 (compile) where

import Control.Monad.State  (runState, state, State(..))
import System.Process       (callCommand)
import Text.Printf          (printf)

import Brainfuck        (Operation(..), Brainfuck)

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
encodeOperation ReadValue           = return "" --TODO implement
encodeOperation (Loop bf)           = state $ \lc ->
    let
        loopStart       = "l" ++ show lc ++ "_start:\ncmpb $0, (%r15, %r14, 1)\nje l" ++ show lc ++ "_end\n\n"
        loopEnd         = "jmp l" ++ show lc ++ "_start\nl" ++ show lc ++ "_end:\n"
        (loopBody, lc') = runState (encodeOperations bf) (lc + 1)
    in
        (loopStart ++ loopBody ++ loopEnd, lc')

assemble :: String -> String -> IO ()
assemble programName assemblyCode = do
    callCommand $ printf "echo '%s' > '%s'.s" assemblyCode programName
    callCommand $ printf "echo '%s' | as -o %s.o" assemblyCode programName
    callCommand $ printf "ld %s.o -o %s" programName programName

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

