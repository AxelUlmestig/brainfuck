{-# LANGUAGE OverloadedStrings #-}

module Compile.Linux.X86_64 (compile) where

import           Prelude                 hiding (unlines)

import           Data.Semigroup          ((<>))
import           Data.String.Interpolate (__i)
import           Data.Text               (Text, pack, unlines)

import           Brainfuck               (AddProd (AddProd), Brainfuck,
                                          Operation (ForLoop, IncrementPointer, IncrementValue, Loop, OutputValue, ReadValue, SetValue))

compile :: Brainfuck -> Text
compile bf =
    [__i|
    .section .bss
        .lcomm memory, 30000

    .section .text
        .global _start

    _printChar:
    movq $1, %rdi       \# stdout file descriptor
    movq $memory, %rsi  \# message to print
    addq %r14, %rsi     \# TODO can this be done in one step?
    movq $1, %rdx       \# message length
    movq $1, %rax       \# sys_write
    syscall
    ret

    _readChar:
    movq $0, %rdi       \# stdin file descriptor
    movq $memory, %rsi  \# message to print
    addq %r14, %rsi     \# TODO can this be done in one step?
    movq $1, %rdx       \# message length
    movq $0, %rax       \# sys_read
    syscall
    ret

    _start:
    movq $memory, %r15
    movq $0, %r14       \# indexn

    #{unlines $ map encodeOperation bf}

    movq $0, %rdi       \# exit code = 0
    movq $60, %rax      \# sys_exit
    syscall
    \n
    |]

encodeOperation :: Operation -> Text
encodeOperation (IncrementPointer n)    = [__i|addq $#{n}, %r14|]
encodeOperation (IncrementValue n)      = [__i|addb $#{n}, (%r15, %r14, 1)|]
encodeOperation OutputValue             = [__i|call _printChar|]
encodeOperation ReadValue               = [__i|call _readChar|]
encodeOperation (SetValue x)            = [__i|movb $#{x}, (%r15, %r14, 1)|]
encodeOperation (ForLoop lId addProds)  =
      [__i|
      \n
      fl#{lId}_start:
      cmpb $0, (%r15, %r14, 1)
      je fl#{lId}_end
      #{unlines (map encodeAddProd addProds)}
      fl#{lId}_end:
      \n
      |]

encodeOperation (Loop lId bf)            =
      [__i|
      \n
      l#{lId}_start:
      cmpb $0, (%r15, %r14, 1)
      je l#{lId}_end
      #{unlines $ map encodeOperation bf}
      jmp l#{lId}_start
      l#{lId}_end:
      \n
      |]

encodeAddProd :: AddProd -> Text
encodeAddProd (AddProd addr fctr) =
  [__i|
  movb $#{fctr}, %al
  mulb (%r15, %r14, 1)
  addb %al, #{addr}(%r15, %r14, 1)
  |]

