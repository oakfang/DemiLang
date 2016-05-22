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

## Modules
You can import modules relative to the importing modules, like so:

```go
// main.dm
import "foo.dm"; // this imports the file "foo.dm" from the same directory main.dm is
```

You can also install demiurges (Demi packages) using [urge](https://github.com/oakfang/urge),
running `urge -i <github_user>/<github_repo>`.

Importing demiurges is done using this syntax (assuming the demiurge's name is Math, for example):

```python
import Math;
```

Notice the lack of path and extension. This will import <running_directory>/urges/Math/main.dm.

*Note:* this time, the starting directory is the one you run `demi` from.

## Creating Demiurges
A demiurge is a github repo containing at the very list 2 files:

- `main.dm` contains the file to be imported.
- `urge.json` is a JSON file containing at the very least a `name` property (in the above example, its value is `"Math"`), and maybe `deps`, which is a list of string in the format <github_user>/<github_repo>`. These dependencies will be installed along with your demiurge.

Done. Now anyone can install your demiurge!

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