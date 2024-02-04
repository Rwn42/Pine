const std = @import("std");

const ast = @import("frontend/ast.zig");
const lexing = @import("./frontend/lexer.zig");
const parsing = @import("./frontend/parser.zig");
const Token = @import("./frontend/token.zig").Token;
const TokenType = @import("./frontend/token.zig").TokenType;
const StringManager = @import("common.zig").StringManager;

pub const TranslationUnit = struct {
    types: usize,
    imports: []TranslationUnit,
};

//compiles the code and returns information about types
pub fn compile_file(input_buffer: []u8, file_name: []const u8, output_fd: std.fs.File, allocator: std.mem.Allocator) ?TranslationUnit {
    _ = output_fd; // autofix
    var sm = StringManager.init(allocator);
    defer sm.destroy();

    var l = lexing.Lexer.init(input_buffer, file_name, &sm) orelse return null;
    var p = parsing.ParserState.init(&l, allocator) orelse return null;
    defer p.deinit();
    const file_ast = p.file_ast();
    _ = file_ast; // autofix
    return null;
}
