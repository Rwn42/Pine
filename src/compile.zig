const std = @import("std");

const ast = @import("frontend/ast.zig");
const lexing = @import("./frontend/lexer.zig");
const parsing = @import("./frontend/parser.zig");
const Token = @import("./frontend/token.zig").Token;
const TokenType = @import("./frontend/token.zig").TokenType;
const StringManager = @import("common.zig").StringManager;

const Types = @import("./backend/typing.zig").Types;
const IR = @import("./backend/ir.zig");
const compile_ir = @import("./backend/linux.zig").compile_ir;

//NOTE: do not recurse through imports for types i dont like it
// if file a needs access to file c it should import it in a not through b
pub const TranslationUnit = struct {
    types: Types,
    imports: []TranslationUnit,
};

//compiles the code and returns information about types
pub fn compile_file(input_buffer: []u8, file_name: []const u8, output_fd: std.fs.File, allocator: std.mem.Allocator) ?void {
    var sm = StringManager.init(allocator);
    defer sm.destroy();

    var l = lexing.Lexer.init(input_buffer, file_name, &sm) orelse return null;
    var p = parsing.ParserState.init(&l, allocator) orelse return null;
    defer p.deinit();
    const file_ast = p.file_ast();

    var tu: TranslationUnit = undefined;

    for (file_ast.imports) |import_tk| {
        std.log.info("skipping imports... not currently implemented {s}", .{import_tk.tag.String});
        //NOTE: eventually could add threading here
        //NOTE: remember to check for cyclic imports
        //NOTE: load file name
        //NOTE: call compile_file
    }

    for (file_ast.records) |r_decl| {
        _ = r_decl;

        //NOTE: do type stuff here
    }

    for (file_ast.functions) |f_decl| {
        _ = f_decl;

        //NOTE: only register the types dont do any compilation
    }

    for (file_ast.constants) |c_decl| {
        std.log.info("skiping constants... not currently implemented {s}", .{c_decl.name_tk});
    }

    //at this point the translation unit is ready to be compiled
    const instructions = IR.generate_ir(tu, file_ast.functions) catch return null;
    compile_ir(instructions, output_fd) catch return null;

    return;
}
