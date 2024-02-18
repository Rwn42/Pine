const std = @import("std");

const typing = @import("backend/typing.zig");
const lexing = @import("frontend/lexer.zig");
const parsing = @import("frontend/parser.zig");
const ir = @import("backend/ir.zig");
const linux = @import("backend/linux.zig");
const ast = @import("frontend/ast.zig");

pub fn compile_file(file_info: *std.StringHashMap(typing.FileTypes), filepath: []const u8, dir: ?[]const u8, allocator: std.mem.Allocator) ?void {
    //open the source file & confirm length
    const contents = open_file(filepath, allocator) orelse return null;

    var lexer = lexing.Lexer.init(contents, filepath, allocator);
    defer lexer.deinit();
    var parser = parsing.ParserState.init(&lexer, allocator) orelse return null;
    defer parser.deinit();

    //generate ast
    const ast_tree = parsing.ast_from_file(&parser) catch |err| {
        allocator.free(contents);
        return panic_or_null(err);
    };
    allocator.free(contents);

    //set up the types
    var file_types = typing.FileTypes.init(allocator) catch |err| return panic_or_null(err);

    //retrieve all the imports
    var import_types_buffer = std.ArrayList(*const typing.FileTypes).init(allocator);
    defer import_types_buffer.deinit();
    for (ast_tree.imports) |import_token| {
        const file = import_token.tag.String;
        const stem = std.fs.path.stem(file);
        var search_path: ?[]const u8 = null;
        if (dir) |dirpath| {
            search_path = std.fs.path.join(allocator, &.{ dirpath, file }) catch |err| return panic_or_null(err);
        }
        defer if (search_path) |path| allocator.free(path);

        if (file_info.contains(stem)) {
            import_types_buffer.append(file_info.getPtr(stem).?) catch |err| return panic_or_null(err);
        } else {
            compile_file(file_info, search_path orelse file, dir, allocator) orelse return null;
            import_types_buffer.append(file_info.getPtr(stem).?) catch |err| return panic_or_null(err);
        }
    }
    file_types.imported_types = import_types_buffer.toOwnedSlice() catch |err| return panic_or_null(err);
    //setup the the local types for the file
    for (ast_tree.records) |record_decl| {
        file_types.register_record(record_decl) catch |err| return panic_or_null(err);
    }

    for (ast_tree.functions) |function_decl| {
        file_types.register_function(function_decl) catch |err| return panic_or_null(err);
    }

    //generate the intermediate representation
    var ir_data = ir.generate_file_ir(file_types, ast_tree, allocator) catch |err| {
        return panic_or_null(err);
    };
    defer ir_data.deinit(allocator);

    std.fs.cwd().deleteDir("out") catch {};
    std.fs.cwd().makeDir("out") catch {};

    const base = std.fs.path.stem(filepath);
    const name = std.fmt.allocPrint(allocator, "{s}/{s}.fasm", .{ "./out", base }) catch {
        @panic("Out of memory!");
    };

    defer allocator.free(name);

    //compile the ir down to fasm file
    linux.fasm_compile(name, ir_data) catch return null;

    file_info.put(base, file_types) catch |err| return panic_or_null(err);
}

fn panic_or_null(err: anyerror) ?void {
    switch (err) {
        error.OutOfMemory => @panic("Out of memory!"),
        else => return null,
    }
}

const MAX_FILE_BYTES = 1024 * 1024;
pub fn open_file(filepath: []const u8, allocator: std.mem.Allocator) ?[]u8 {
    const file_buffer = std.fs.cwd().readFileAlloc(allocator, filepath, MAX_FILE_BYTES) catch |err| {
        switch (err) {
            error.FileTooBig => std.log.err("File {s} is to big try splitting your program into more files.", .{filepath}),
            else => std.log.err("File {s} was not found.", .{filepath}),
        }
        return null;
    };

    if (file_buffer.len == 0) {
        std.log.err("File {s} was found but is empty", .{filepath});
        allocator.free(file_buffer);
        return null;
    }

    return file_buffer;
}
