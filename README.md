# Advent of Code 2021

Using https://racket-lang.org/

Install on Mac with Homebrew
```
brew install --cask racket
```
Run code
```
racket -e '(enter! "1/1.rkt")' -i
```
will start the Racket REPL and load code from the file `1/1.rkt`.
Run any function
```
> (part1Solver input)
1184
> (part1Solver (list 1 2 3))
2
```
Exit REPL with
```
> (exit)
```