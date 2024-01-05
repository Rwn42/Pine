# Osmium
An attempt at a simple procedural programming language written in Zig. The language design was based on Jai, Odin, Go, Zig, Rust, C and OCaml in no particular order.

## Current Syntax
```

my_const :: 42; //immutable, comptime known constant
//mutable global state not implemented may never be implemented

//pointers use the ^ instead of * but address of still &
add :: fn(x: int, y: ^int) int {
    return x + ^y;
}

//ifs and whiles work like other languages
//for loop is not implemented yet
if 3 == 3 {
    x := 4;
    add(1, &x); //should be 5

    i: int = 0;
    while i != add(1, &x) {
        i = i + 1;
    }
}

my_array: [10]int = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
2 := my_array.(1+1); //to access array use .index or .(index expr)

x: string = "hello, world!" //strings are supported
//no escape sequences yet.

```

## Current Goal
get to a turing complete stage capable of printing hello, world!
- [x] lexer
- [ ] parser
- [ ] ir gen
- [ ] interpreter
- [ ] native compiler
- [ ] type checking (may introduce another ast / ir)

