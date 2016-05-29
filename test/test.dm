/*
Currently, stdlib contains:
print(ARG) => prints to stdout
read("int") => read an integer from stdin
read("float") => read a floating point number from stdin
read("bool") => read a boolean from stdin
read("str") => read a string from stdin
*/

greet = fn(name) {
    .print("Hello, " + name + "!");
};

opFn = fn(x, op, y) {
    if op == "+" return=x + y
    else if op == "-" return=x - y
    else if op == "*" return=x * y
    else if op == "/" return=x / y
    else {
        .print("Unsupported operator")
    }
};

.greet("world");

.print("Enter first number:");
a = read("float");

.print("Enter operator (+, -, *, /)");
op = read("str");

.print("Enter second number:");
b = read("float");
.print(opFn(a, op, b));