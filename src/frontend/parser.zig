const std = @import("std");

const ast = @import("ast.zig");
const lexing = @import("lexer.zig");

const ParseError = error{
    OutOfMemory,
    Unexpected,
};

pub fn ast_from_file(filepath: []const u8, file_buffer: []u8, allocator: std.mem.Allocator) !ast.AST {
    const lexer = lexing.Lexer.init(file_buffer, filepath, allocator);
    _ = lexer;

    return ParseError.Unexpected;
}
