# MindBlowing
A brainfuck compiler written in Haskell.

## How to use
Just run the Main.hs file with the --help flag.

## Motivation
Mainly for fun, but also I wanted to get better at haskell and x86 (completely different things, but still), so this is a project that would combine both (haven't written the assembly part yet tho..).

## About
- It has an interpreter, that can both interpret a file or interpret code as you type it in the console.
- I wrote a specific bytecode for it so that it is easier to optimize the bf code.
- Currently it compiles to bytecode and to C.
- Optimizations are passed as flags and can be passed as both levels (-O1, -O2..) or individually with it's respective flag (shown in the help command).
- I wrote the args parser really quick, don't expect a lot from it, lol.

## TODO
- Compile to x86 and maybe to ELF.
