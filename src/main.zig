const std = @import("std");

const lexing = @import("lexer.zig");
const parsing = @import("parser.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const StringManager = @import("common.zig").StringManager;

const MAX_FILE_BYTES = 1024 * 1024;

pub fn main() !void {
    //setting stuff up
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    //read in cli options
    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const cli_options = CLIOptions.create(args) catch |err| {
        switch (err) {
            CLIOptions.CLIError.NoInput => std.log.err("No input file provided", .{}),
            CLIOptions.CLIError.NoOutput => std.log.err("No output file provided after -o", .{}),
            CLIOptions.CLIError.ConflictingFlag => std.log.err("Two compiler flags conflict... cannot use -l and -p", .{}),
        }
        try usage(stdout);
        try bw.flush();
        return;
    };

    //prepare buffers and string manager

    const file_buffer = std.fs.cwd().readFileAlloc(allocator, cli_options.input_file, MAX_FILE_BYTES) catch |err| {
        switch (err) {
            error.FileTooBig => std.log.err("File {s} is to big try splitting your program into more files.", .{cli_options.input_file}),
            else => std.log.err("File {s} was not found.", .{cli_options.input_file}),
        }
        return;
    };

    defer allocator.free(file_buffer);

    if (file_buffer.len == 0) {
        std.log.err("File {s} was found but is empty", .{cli_options.input_file});
        return;
    }

    var sm = StringManager.init(allocator);
    defer sm.destroy();

    //compilation begins here

    var l = lexing.Lexer.init(file_buffer, cli_options.input_file, &sm) orelse return;
    if (cli_options.lex_only) {
        try print_lexer(stdout, &l);
        try bw.flush();
        return;
    }

    var p = parsing.ParserState.init(&l, allocator) orelse return;
    if (cli_options.parse_only) {
        @panic("Not Implemented");
    }

    p.parse();
    defer p.deinit();

    try stdout.print("{any} \n", .{p.top_level[0]});
    try bw.flush();
}

//wanted to use argIterator here but i couldnt get it to work
const CLIOptions = struct {
    input_file: []const u8,
    output_file: []const u8,
    pos: usize,
    lex_only: bool,
    parse_only: bool,
    native: bool,

    pub const CLIError = error{
        NoInput,
        NoOutput,
        ConflictingFlag,
    };

    pub fn create(args: [][]u8) !CLIOptions {
        var opt = CLIOptions{
            .input_file = undefined,
            .output_file = "default",
            .pos = 0,
            .lex_only = false,
            .parse_only = false,
            .native = false,
        };

        opt.pos += 1; //skip program name

        if (opt.pos >= args.len) return CLIError.NoInput;
        opt.input_file = args[opt.pos];
        opt.pos += 1;

        while (opt.pos <= args.len - 1) : (opt.pos += 1) {
            if (std.mem.eql(u8, "-o", args[opt.pos])) {
                opt.pos += 1;
                if (opt.pos >= args.len) return CLIError.NoOutput;
                opt.output_file = args[opt.pos];
            } else if (std.mem.eql(u8, "-l", args[opt.pos])) {
                if (opt.parse_only) return CLIError.ConflictingFlag;
                opt.lex_only = true;
            } else if (std.mem.eql(u8, "-p", args[opt.pos])) {
                if (opt.lex_only) return CLIError.ConflictingFlag;
                opt.parse_only = true;
            } else if (std.mem.eql(u8, "-cc", args[opt.pos])) {
                opt.native = true;
            }
        }

        return opt;
    }
};

fn usage(writer: anytype) !void {
    try writer.print("Usage: osmium [main_file] [flags]\n", .{});
    try writer.print("  flags:\n", .{});
    try writer.print("  -o [output file]: specify an output file follow this flag with a filename\n", .{});
    try writer.print("  -cc: compile to native code\n", .{});
    try writer.print("  -l: only perform lexical analysis (mainly for compiler debugging purposes)\n", .{});
    try writer.print("  -p: only generate the AST (mainly for compiler debugging purposes)\n", .{});
    try writer.print("ex: osmium main.os -cc -o hello.exe\n", .{});
}

fn print_lexer(writer: anytype, l: *lexing.Lexer) !void {
    var t = l.next() orelse return;
    while (t.tag != .EOF) : (t = l.next() orelse return) {
        try writer.print("{s}\n", .{t});
    }
}
