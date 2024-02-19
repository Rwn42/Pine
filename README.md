# Pine
Pine, fomerly called Osmium, is an at *attempt* at a simple C like language written in Zig. The lexer and parser are custom, expression parsing based on Writing an Interpreter in Go book. Many thanks to Tsoding for all the useful tidbits about stack based code and fasm.

## Current TODO
- [] overhaul foreign declarations
    - [] know types
    - [] track library name to auto link

- [] clean up the IR to prepare for adding the rest of the language
- [] add the rest of the current parsed language to IR

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
- [] namespacing *ugh*
- [] enums & unions
- [] interpret the IR

## Milestone 3
**Final Extras**
Subject to change.
- [] type system overhaul (one of: generics, compile time code execution, macros)
- [] support for basic items without libc (printing, malloc ect)
- [] windows compilation support

## Milestone 4
**Adjacent items to core language building, probably will not implement them**
- [] different Backend (QBE, Tilde I wont touch LLVM)
- [] self hosting
- [] some sort of memory manegement gimmick
- [] skip fasm and generate object files directly
- [] compile speed/space optimizations (might pick away at these for fun)

## Current Hello, World!
Subject to change.
```
#foreign "libc" ["printf"]

main :: fn() {
    printf("Hello, World!")
}
```

## Technical Details
If anyone stumbles upon this project and is curious, or if I forget what I did the details are below.

### General Architecture
Pine tries to have a simple compilation process each file goes through the following process:
lex -> parse -> types -> IR -> assembly -> object files -> executable.
Pine will eventually, or already does depending on if I updated this, have compile time code execution by interpreting the ir.

Pine has a stack based IR which is absolutely terrible for efficent codegen without translating the IR into something else. However, the IR is easy to generate, interpret and reason about. Eventually I hope to optimize the stack based IR to reduce some of the reduncy.

### Machine Code Generation
Pine's calling convention is not the same as C's, but it can be if importing a C function.
Pine reserves space for return value, then places params. the caller must clean up the params so only return value
is left. Everything is passed by value, arrays can be passed by pointer to the start or by passing a slice.

Pine reserves the first N bytes of any stack frame for local variables the rest is used for temporary expressions
r10 is used as tempory storage when loading or storing complex data.
r11 is used to store the stack pointer if the stack needs to be temporarily alligned for a cdecl call
the standard C calling convention registers are considered volatile as they may be used at any point to call into C.

## Language Reference
TODO