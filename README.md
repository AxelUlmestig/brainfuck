# Brainfuck

### Introduction

Brainfuck is a very small and confusing programming language. [This Wikipedia
article](https://en.wikipedia.org/wiki/Brainfuck) gives a pretty good summary
of it.

This repository contains a Brainfuck compiler written in Haskell which is a
very large and confusing programming language.

### Setup

In order to run it you must first install the
[Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) build
tool.

Run the following command to build and install the brainfuck interpreter on
your computer.
```bash
$ stack install
```

This will install a CLI tool called brainfuck.

### Running brainfuck code

Now we can run a Hello World example of Brainfuck. First copy the program found
[here](https://en.wikipedia.org/wiki/Brainfuck#Hello_World!)
and save it in a file called hello.bf. Then run the interpreter with the file.
```bash
$ brainfuck run hello.bf
```
Which should result in the following output:
```bash
$ brainfuck run hello.bf
Hello World!
```

### Compiling brainfuck code

The current version only supports compiling to Linux distributions with x86-64
architecture.

```bash
$ brainfuck compile hello.bf
```

will produce an executable called `hello`
```bash
$ ./hello
Hello World!
```

### Optimizations

The brainfuck code goes through the following optimzation steps before it gets
run/compiled.

  - **Combine consecutive `+` and `-`** \
    `++-` will be converted into a command that adds 1 to the current cell.

  - **Combine consecutive `>` and `<`** \
    `>><` will be converted into a command that moves the pointer one step to
    the right.

  - **Remove loops that don't contain pointer movement or IO** \
    Any loops that only contains `+-[]` will always reset the current cell to
    `0` and then move on. These loops are replaced with a command that sets the
    current cell to `0`.

  - **Remove loops that are never entered** \
    If there are consecutive loops then the loops after the first one will
    never be run and can be safely removed.

  - **Remove initial loops** \
    It's common to add an initial loop in brainfuck source files to be able to
    write comments without having to worry about using the special characters
    `+-<>,.[]`. These can be safely removed since they will never be entered.

  - **Simplify multiplication loops** \
    Loops that only contain `<`, `>`, `+` and `-` where the total number of
    pointer shifts add up to `0` and the increments to the initial cell add up
    to `-1` work like multiplication.

    E.g. `[->+>+++<<]` will result in
    ```C
    *(p + 1) += *p * 1
    *(p + 2) += *p * 3
    *p       = 0
    ```

    (where `p` is the memory pointer and `*p` is the value that it points to)

### Debugging

If you want to debug the executables (and you're a masochist) you an compile
with the `--debug` flag and then use `gdb` to step through the assembly code in
the executable.

```bash
$ brainfuck compile hello.bf --debug
$ gdb hello
```
