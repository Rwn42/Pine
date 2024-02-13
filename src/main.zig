const std = @import("std");

const lexing = @import("frontend/lexer.zig");
const parsing = @import("frontend/parser.zig");
const typing = @import("backend/typing.zig");
const ir = @import("backend/ir.zig");
const linux = @import("backend/linux.zig");

const ast = @import("frontend/ast.zig");

pub fn main() void {
    //this allocator will be used for everything for now
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    //prepare stdout mainly for printing compiler usage
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    defer bw.flush() catch {};
    const stdout = bw.writer();

    //CLI parsing
    var args = std.process.ArgIterator.initWithAllocator(allocator) catch {
        @panic("Out of memory!");
    };
    const opt = Options.from_iterator(&args, stdout) orelse return;

    if (opt.lex) output_lexer(opt.main_file, allocator);
    if (opt.parse) output_parser(opt.main_file, allocator);

    //compile the main file here
    compile_file(opt.main_file, allocator) orelse {
        std.log.info("exiting...", .{});
        return;
    };

    //NOTE: here is where we would call into fasm and ld for final executable
}

fn compile_file(filepath: []const u8, allocator: std.mem.Allocator) ?void {
    //open the source file & confirm length
    const contents = open_file(filepath, allocator) orelse return null;

    var lexer = lexing.Lexer.init(contents, filepath, allocator);
    defer lexer.deinit();
    var parser = parsing.ParserState.init(&lexer, allocator) orelse return null;
    defer parser.deinit();

    //generate ast
    const ast_tree = parsing.ast_from_file(&parser) catch |err| {
        allocator.free(contents);
        switch (err) {
            error.OutOfMemory => @panic("Out of memory!"),
            else => return null,
        }
    };
    allocator.free(contents);

    //NOTE: handle imports here we will need some sort of global compilation state i presume

    //load custom types for the file
    var file_types = typing.Types.init(allocator) catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("Out of memory!"),
            else => return null,
        }
    };

    for (ast_tree.records) |record_decl| {
        file_types.register_record(record_decl) catch |err| {
            switch (err) {
                error.OutOfMemory => @panic("Out of memory!"),
                else => return null,
            }
        };
    }

    for (ast_tree.functions) |function_decl| {
        file_types.register_function(function_decl) catch |err| {
            switch (err) {
                error.OutOfMemory => @panic("Out of memory!"),
                else => return null,
            }
        };
    }

    //generate the intermediate representation
    var irgen = ir.IRGenerator.init(ast_tree, file_types, allocator);
    var ir_data = irgen.generate() catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("Out of memory!"),
            else => return null,
        }
    };
    defer irgen.deinit();
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
}

const MAX_FILE_BYTES = 1024 * 1024;
fn open_file(filepath: []const u8, allocator: std.mem.Allocator) ?[]u8 {
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

const Options = struct {
    main_file: []const u8,
    output_name: []const u8,
    lex: bool,
    parse: bool,

    fn from_iterator(args: *std.process.ArgIterator, writer: anytype) ?Options {
        var opt: Options = undefined;
        opt.output_name = "pine_default_output";
        _ = args.next() orelse return null; //skip program name
        opt.main_file = args.next() orelse {
            std.log.err("No main file specified", .{});
            return null;
        };

        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "-o")) {
                opt.output_name = args.next() orelse {
                    std.log.err("No output file specified after -o", .{});
                    usage(writer) catch return null;
                    return null;
                };
            } else if (std.mem.eql(u8, arg, "-lex")) {
                opt.lex = true;
            } else if (std.mem.eql(u8, arg, "-parse")) {
                opt.parse = true;
            } else {
                std.log.err("Unknown flag {s}", .{arg});
                usage(writer) catch return null;
                return null;
            }
        }

        return opt;
    }

    fn usage(writer: anytype) !void {
        try writer.print("Usage: pine [main_file] [flags]\n", .{});
        try writer.print("  flags:\n", .{});
        try writer.print("  -o [output file]: specify an output file follow this flag with a filename\n", .{});
        try writer.print("  -lex: produce output of lexer \n", .{});
        try writer.print("  -parse: produce output of parser \n", .{});
        try writer.print("ex: pine main.pine -o hello.exe\n", .{});
    }
};

fn output_lexer(filepath: []const u8, allocator: std.mem.Allocator) void {
    const contents = open_file(filepath, allocator) orelse return;
    defer allocator.free(contents);

    const output_fd = std.fs.cwd().createFile("debug_lex.txt", .{}) catch {
        std.log.err("Output file {s} could not be created", .{"debug_lex.txt"});
        return;
    };
    defer output_fd.close();

    var lexer = lexing.Lexer.init(contents, filepath, allocator);
    defer lexer.deinit();
    while (lexer.next()) |token| {
        if (token.tag == .EOF) break;
        output_fd.writer().print("{s}\n", .{token}) catch return;
    }
}

fn output_parser(filepath: []const u8, allocator: std.mem.Allocator) void {
    const contents = open_file(filepath, allocator) orelse return;
    defer allocator.free(contents);

    const output_fd = std.fs.cwd().createFile("debug_parse.txt", .{}) catch {
        std.log.err("Output file {s} could not be created", .{"debug_parse.txt"});
        return;
    };
    defer output_fd.close();

    const writer = output_fd.writer();

    var lexer = lexing.Lexer.init(contents, filepath, allocator);
    defer lexer.deinit();

    var parser = parsing.ParserState.init(&lexer, allocator) orelse return;
    defer parser.deinit();

    const ast_tree = parsing.ast_from_file(&parser) catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("Out of memory!"),
            else => return,
        }
    };

    for (ast_tree.functions) |v| writer.print("{s}", .{ast.Declaration{ .FunctionDeclaration = v }}) catch return;
    for (ast_tree.foreign) |v| writer.print("{s}", .{ast.Declaration{ .ForeignDeclaration = v }}) catch return;
    for (ast_tree.imports) |v| writer.print("importing: {s}", .{v}) catch return;
    for (ast_tree.constants) |v| writer.print("{s}", .{ast.Declaration{ .ConstantDeclaration = v }}) catch return;
    for (ast_tree.records) |v| writer.print("{s}", .{ast.Declaration{ .RecordDeclaration = v }}) catch return;
}
