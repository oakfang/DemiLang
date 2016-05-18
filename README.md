# DemiLang
A basic attempt at a haskell-interpreted language

## Demo code
```
a := 5;
b := 3;
(if a > b then
    while b < (a * 3) do (
        c := 1;
        b := b + c;
); else skip);
print b;
```