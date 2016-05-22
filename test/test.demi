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