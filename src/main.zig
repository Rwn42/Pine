const std = @import("std");

const lexing = @import("frontend/lexer.zig");
const parsing = @import("frontend/parser.zig");
const ast = @import("frontend/ast.zig");
const typing = @import("backend/typing.zig");
const linux = @import("backend/linux.zig");

const compile_file = @import("compile.zig").compile_file;
const open_file = @import("compile.zig").open_file;

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

    var file_info = std.StringHashMap(typing.FileTypes).init(allocator);
    defer file_info.deinit();

    defer {
        var v_iter = file_info.valueIterator();
        while (v_iter.next()) |types| {
            types.deinit();
        }
    }

    //compile the main file here
    compile_file(&file_info, opt.main_file, allocator) orelse {
        std.log.info("exiting...", .{});
        return;
    };

    linux.fasm_runtime() catch return;

    //NOTE: here is where we would call into fasm and ld for final executable
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
