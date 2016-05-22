# DemiLang
A basic attempt at a haskell-interpreted language

## Building
```
git clone ...
cabal install
cabal build
alias demi=./dist/build/demi/demi.exe # used on windows GitBash
```

## Usage
```
Usage:
demi.exe parse <filepath> -- print AST symbols (use for exec)
demi.exe [run] <filepath> -- parse and run the file on the fly
demi.exe exec <filepath>  -- run a pre-parsed AST symbols file
demi.exe                  -- run demi in stdin interpreter mode
```

## Demo code
```c
/*
Currently, stdlib contains:
print[ARG] => prints to stdout
read["int"] => read an integer from stdin
read["bool"] => read a boolean from stdin
read["str"] => read a string from stdin
*/

greet = fn[name] {
    .print["Hello, " + name + "!"];
};

opFn = fn[op] {
    if op == "+" return=fn[x] {return=fn[y] {return=x + y}}
    else if op == "-" return=fn[x] {return=fn[y] {return=x - y}}
    else if op == "*" return=fn[x] {return=fn[y] {return=x * y}}
    else if op == "/" return=fn[x] {return=fn[y] {return=x / y}}
    else {
        .print["Unsupported operator"]
    }
};

.greet["world"];

.print["Enter first number:"];
a = read["int"];

.print["Enter operator (+, -, *, /)"];
op = read["str"];

.print["Enter second number:"];
b = read["int"];

step1 = opFn[op];
should step1 {
    step2 = step1[a];
    .print[step2[b]];
};
```