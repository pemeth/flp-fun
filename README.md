# FLP project 1 - plg-2-nka
## Patrik Németh (xnemet04)

### Overview
The output of this project is a haskell program that is capable of
transforming right linear grammars into non-deterministic finite-state automata.

The source code is divided into multiple files:
- Main.hs   contains the program entrypoint
- Input.hs  contains input parsing functions
- Lib.hs    contains data definitions and useful general functions
- Algos.hs  contains algorithms for grammar transformations

### Input syntax
- A line containing a list of non-terminals separated by commas
- A line containing a list of terminals separated by commas
- A line containing the starting non-terminal
- Multiple lines of grammar rules in the form of A->x or A->xB or A->#,
  where A, B \in {non-terminals}, x \in {terminals}*, and # is an empty symbol.
  One rule per line.

Example:
    A,B
    a,b,c
    A
    A->aaB
    A->ccB
    B->bB
    B->#
    B->b

### Program usage:
    ./flp21-fun OPTION [INPUT]
        OPTION is one of:
            -i  Print out the loaded grammar
            -1  Print out the grammar based on theorem 3.2 from TIN
            -2  Print out the finite state automata based on theorem 3.6 from TIN
        INPUT is an optional file, in which the input grammar is stored.
        If INPUT is not defined, the grammar is loaded from stdin.

### Test files
Test files in the `test` directory have the following format:
- *.in    are input files with defined grammars.
- *-i.out are program outputs with option -i.
- *-1.out are program outputs with option -1.
- *-2.out are program outputs with option -2.
