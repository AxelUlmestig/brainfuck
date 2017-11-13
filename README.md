# Brainfuck
Brainfuck is a very small and confusing programming language. This [Wikipedia
article](https://en.wikipedia.org/wiki/Brainfuck) gives a pretty good summary
of it.

This repo contains an interpreter for Brainfuck written in the very large and
confusing programming language Haskell. In order to run it you must first
install the
[Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) build
tool.

Run the following command to build and install the brainfuck interpreter on
your computer.
```bash
$ stack install
```

Now we can run a Hello World example of Brainfuck. First copy the program found
[here](https://esolangs.org/wiki/Hello_world_program_in_esoteric_languages#Brainfuck)
and save it in a file called hello.bf. Then run the interpreter with the file.
```bash
$ brainfuck hello.bf
```
Which should result in the following output:
```bash
$ brainfuck hello.bf
Hello World!
```
