# DemiLang
A basic attempt at a haskell-interpreted language

## Building
```
git clone ...
cabal build
alias demi=./dist/build/demi/demi.exe # used on windows GitBash
```

## Usage
```
Usage:
demi.exe <parse|run|exec> <filepath>
demi.exe parse <filepath> -- print AST symbols (use for exec)
demi.exe [run] <filepath> -- parse and run the file on the fly
demi.exe exec <filepath> -- run a pre-parsed AST symbols file
```

## Demo code
```
print "Hello, world!";
a = 5;
b = 3;
if a > b {
    while b < (a * 3) {
        c = 1;
        b = b + c;
    }
} else if a == b print 7
  else print 9;
print b; // prints 15
```