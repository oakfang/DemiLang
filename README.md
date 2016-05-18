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
a := 5;
b := 3;
(
    if a > b then
        while b < (a * 3) do (
            c := 1;
            b := b + c;
        );
    else skip;
);
print b; // prints 15
```