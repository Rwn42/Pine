const std = @import("std");

const lexing = @import("frontend/lexer.zig");
const parsing = @import("frontend/parser.zig");

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

    if (opt.lex) output_lexer(opt.main_file, opt.output_name, allocator);

    //compile the main file here
    compile_file(opt.main_file, allocator) orelse return;

    //NOTE: here is where we would call into fasm and ld for final executable
}

fn compile_file(filepath: []const u8, allocator: std.mem.Allocator) ?void {
    //open the source file & confirm length
    const contents = open_file(filepath, allocator) orelse return;

    //generate ast
    const ast = parsing.ast_from_file(filepath, contents, allocator) catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("Out of memory!"),
            else => return null,
        }
    };
    allocator.free(contents);

    _ = ast;

    //NOTE: prepare for IR (construct context for types, functions ect)

    //NOTE: generate IR

    //NOTE: compile to fasm

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

fn output_lexer(filepath: []const u8, output_path: []const u8, allocator: std.mem.Allocator) void {
    const contents = open_file(filepath, allocator) orelse return;
    defer allocator.free(contents);

    const output_fd = std.fs.cwd().createFile(output_path, .{}) catch {
        std.log.err("Output file {s} could not be created", .{output_path});
        return;
    };
    defer output_fd.close();

    var lexer = lexing.Lexer.init(contents, filepath, allocator);
    while (lexer.next()) |token| {
        if (token.tag == .EOF) break;
        var bw = std.io.bufferedWriter(output_fd.writer());
        const w = bw.writer();
        w.print("{s}\n", .{token}) catch return;
        bw.flush() catch return;
    }
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
