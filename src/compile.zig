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
const TypeError = @import("./backend/typing.zig").TypeError;

//NOTE: do not recurse through imports for types i dont like it
// if file a needs access to file c it should import it in a not through b
pub const TranslationUnit = struct {
    types: Types,
    file: []const u8,
    imports: []TranslationUnit,
};

const CompilationError = error{
    Failed,
};

//compiles the code and returns information about types
pub fn compile_file(input_buffer: []u8, file_name: []const u8, output_fd: std.fs.File, allocator: std.mem.Allocator) anyerror!TranslationUnit {
    var sm = StringManager.init(allocator);
    defer sm.destroy();

    var l = lexing.Lexer.init(input_buffer, file_name, &sm) orelse return CompilationError.Failed;
    var p = parsing.ParserState.init(&l, allocator) orelse return CompilationError.Failed;
    defer p.deinit();
    const file_ast = p.file_ast();

    var tu: TranslationUnit = .{
        .types = Types.init(allocator),
        .file = file_name,
        .imports = undefined,
    };
    errdefer tu.types.deinit();

    for (file_ast.imports) |import_tk| {
        std.log.info("skipping imports... not currently implemented {s}", .{import_tk.tag.String});
        //NOTE: eventually could add threading here
        //NOTE: remember to check for cyclic imports
        //NOTE: load file name
        //NOTE: call compile_file
    }

    for (file_ast.records) |r_decl| {
        tu.types.register_record(r_decl.*) catch |err| switch (err) {
            TypeError.OutOfMemory => {
                @panic("FATAL COMPILER ERROR: Out of memory");
            },
            else => return err,
        };
    }

    for (file_ast.functions) |f_decl| {
        tu.types.register_function(f_decl.*) catch |err| switch (err) {
            TypeError.OutOfMemory => {
                @panic("FATAL COMPILER ERROR: Out of memory");
            },
            else => return err,
        };
    }

    for (file_ast.constants) |c_decl| {
        std.log.info("skiping constants... not currently implemented {s}", .{c_decl.name_tk});
    }

    //at this point the translation unit is ready to be compiled
    const instructions = try IR.generate_ir(tu, file_ast.functions, allocator);
    defer allocator.free(instructions);
    try compile_ir(instructions, output_fd);

    return tu;
}
