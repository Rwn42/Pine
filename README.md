# Pine
Pine, fomerly called Osmium, is an at *attempt* at a simple C like language written in Zig. The lexer and parser are custom, expression parsing based on Writing an Interpreter in Go book.

## Milestone 1
**Native "Hello, World!" application**
I want to fully support variables, control flow, records, functions, arrays, records and slices for this version
- [x] port lexer from old version
- [x] port parser from old version
- [] intermediate representation
- [] native compilation

## Milestone 2
**Similiar in convience to C, excluding stdlib** 
for this milestone I aim to have the rest of the standard C like language features as well as an interpreter.
Subject to change.
- [] enums & unions
- [] interpret the IR

## Milestone 3
**Final Extras**
Subject to change.
- [] type system overhaul (one of: generics, compile time code execution, macros)
- [] windows compilation support

## Current Hello, World! Plan
Subject to change.
```
#foreign "libc" ["printf"]

main :: fn() {
    msg = c"Hello, World!" //c here to make it a c string
    printf(msg)
}
```

