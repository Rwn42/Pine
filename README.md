# Osmium
An attempt at a simple procedural programming language written in Zig. The language design was based on Jai, Odin, Go, Zig, Rust, C and OCaml in no particular order.

## Current Syntax
The below is more of a reminder for myself while developing it is not yet finalized, nor explained well.

### Top Level Code
For now, only three things can be outside of functions
```odin
some_const :: 42; //must be comptime known and is immutable

//return type is nessecary for now
some_function :: fn(x: int) int { ... }

some_record :: record { //like structs in C
    x: int,
    y: ^^int,
}

```

### Statements and Expressions
Osmium is not a expression based language the semantics of if statements and while loops mirror C.
Semicolons are mandatory after expressions but i may switch to semicolon or newlines down the road.

```odin
if x < 2 {
    return 5;
}

i: int = 0; //explicit type
y := 0; //infered type

while i < 10 {
    i = i + 1 // += not implemented yet
}

some_function(); //ok
hello; //ok but useless
1 + hello; // not ok random expressions arent allowed as a statement unless they start with an identifier
```

Currently osmium only has arrays and records but i plan on adding slices, enums and unions and/or interfaces.
Records do not have methods but i may implement such a feature in the future
```odin
Vector2 :: record {
    x: float,
    y: float,
}

my_vec: Vector2 = {x: 1, b: 2};


my_array: [10]int = [1, 2, 3, 4, 5, 6];
my_array.0 = 1;
x: int = 5;
x = my_array.(x); //parenthesis required for array index most of the time

```

### Types and Operators
Types are not yet finalized however many operators are.
```
y: int = 10 + (2 * 3 + 4) - 6; //parsed correctly according to bedmas
x: bool = !(y < 2);
p: ^int = &(^(&y)) //& is reference ^ is dereference and for pointer types

```

## Current Goal
get to a turing complete stage capable of printing hello, world!
- [x] lexer
- [x] parser
- [x] ir gen
- [x] interpreter
- [ ] native compiler
- [ ] type checking (may introduce another ast / ir)

## Afterwords
- [ ] File includes
- [ ] Generics or similair concepts
- [ ] Heap allocation
- [ ] FFI (possibly for a print function)
- [ ] namespacing ? 