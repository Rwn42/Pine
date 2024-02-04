const std = @import("std");

const AST = @import("../frontend/ast.zig");

const TypeError = error{
    OutOfMemory,
    Undeclared,
    Duplicate,
};
