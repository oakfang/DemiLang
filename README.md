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
demi.exe parse <filepath> -- print AST symbols (use for exec)
demi.exe [run] <filepath> -- parse and run the file on the fly
demi.exe exec <filepath>  -- run a pre-parsed AST symbols file
demi.exe                  -- run demi in stdin interpreter mode
```

## Demo code
```
greet = fn [name] {
    print "Hello, " + name + "!";
};

do call greet["world"];
f = fn [x] {
    print x;
};
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
hello = "hello " + "world!";
should hello == "hello world!" {
    do call f[5];
    print "I'm gonna fail now, with a red error";
    print x;
}
```