# Pine
Pine, fomerly called Osmium, is an at *attempt* at a simple C like language written in Zig. The lexer and parser are custom, expression parsing based on Writing an Interpreter in Go book. Many thanks to Tsoding for all the useful tidbits about stack based code.

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
    msg: cstring = "hello, world"
    printf(msg)
}
```

## Technical Details
If anyone stumbles upon this project and is curious, or if I forget what I did the details are below.

### General Architecture
Pine tries to have a simple compilation process each file goes through the following process:
lex -> parse -> types -> IR -> assembly -> object files -> executable.
Pine will eventually, or already does depending on if I updated this, have compile time code execution by interpreting the ir.

Pine has a stack based IR which is absolutely terrible for efficent codegen without translating the IR into something else. However, the IR is easy to generate, interpret and reason about. Eventually I hope to optimize the stack based IR to reduce all the reduncy.

### Machine Code Generation
Pine's calling convention is not the same as C's, but it can be if importing a C function.
Pine reserves space for return value, then places params. the caller must clean up the params so only return value
is left.

Pine reserves the first N bytes of any stack frame for local variables the rest is used for temporary expressions
r10 is the only special register for pine as it is used as tempory storage when loading or storing complex data
the standard C calling convention registers are considered volatile as they may be used at any point to call into C.

## Language Reference
TODO