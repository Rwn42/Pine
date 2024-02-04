const std = @import("std");

const lexing = @import("./frontend/lexer.zig");
const parsing = @import("./frontend/parser.zig");
const Token = @import("./frontend/token.zig").Token;
const TokenType = @import("./frontend/token.zig").TokenType;
const StringManager = @import("common.zig").StringManager;
const compile_file = @import("compile.zig").compile_file;

const MAX_FILE_BYTES = 1024 * 1024;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    //prepare stdout writer
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    defer bw.flush() catch {};
    const stdout = bw.writer();

    //load in CLI
    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const cli_options = CLIOptions.create(args) catch |err| {
        switch (err) {
            CLIOptions.CLIError.NoInput => std.log.err("No input file provided", .{}),
            CLIOptions.CLIError.NoOutput => std.log.err("No output file provided after -o", .{}),
            CLIOptions.CLIError.ConflictingFlag => std.log.err("Two compiler flags conflict... cannot use -lex and -parse", .{}),
            else => {},
        }
        try usage(stdout);
        return;
    };

    //open the output file for writing
    const output_fd = std.fs.cwd().createFile(cli_options.output_file, .{}) catch {
        std.log.err("Output file {s} could not be created", .{cli_options.output_file});
        return;
    };
    defer output_fd.close();

    //open the input file for reading
    const file_buffer = std.fs.cwd().readFileAlloc(allocator, cli_options.input_file, MAX_FILE_BYTES) catch |err| {
        switch (err) {
            error.FileTooBig => std.log.err("File {s} is to big try splitting your program into more files.", .{cli_options.input_file}),
            else => std.log.err("File {s} was not found.", .{cli_options.input_file}),
        }
        return;
    };
    defer allocator.free(file_buffer); //eventually this can be freed after parsing
    if (file_buffer.len == 0) {
        std.log.err("File {s} was found but is empty", .{cli_options.input_file});
        return;
    }

    if (cli_options.lex_only or cli_options.parse_only) {
        var output_buffer = std.io.bufferedWriter(output_fd.writer());
        defer output_buffer.flush() catch {};
        const output_writer = output_buffer.writer();

        std.log.info("Parsing...", .{});
        var sm = StringManager.init(allocator);
        defer sm.destroy();
        var l = lexing.Lexer.init(file_buffer, cli_options.input_file, &sm) orelse return;

        if (cli_options.lex_only) {
            try print_lexer(output_writer, &l);
            return;
        }

        var p = parsing.ParserState.init(&l, allocator) orelse return;
        p.parse();
        defer p.deinit();
        try print_parser(output_writer, &p);
        return;
    }

    _ = compile_file(file_buffer, cli_options.input_file, output_fd, allocator);
}

//wanted to use argIterator here but i couldnt get it to work
pub const CLIOptions = struct {
    input_file: []const u8,
    output_file: []const u8,
    pos: usize = 1,
    lex_only: bool = false,
    parse_only: bool = false,

    pub const CLIError = error{
        NoInput,
        NoOutput,
        ConflictingFlag,
        FlagDoesNotExist,
    };

    pub fn create(args: [][]u8) !CLIOptions {
        var opt = CLIOptions{
            .input_file = undefined,
            .output_file = "default",
        };

        if (opt.pos >= args.len) return CLIError.NoInput;
        opt.input_file = args[opt.pos];
        opt.pos += 1;

        while (opt.pos <= args.len - 1) : (opt.pos += 1) {
            if (std.mem.eql(u8, "-o", args[opt.pos])) {
                opt.pos += 1;
                if (opt.pos >= args.len) return CLIError.NoOutput;
                opt.output_file = args[opt.pos];
            } else if (std.mem.eql(u8, "-lex", args[opt.pos])) {
                if (opt.parse_only) return CLIError.ConflictingFlag;
                opt.lex_only = true;
            } else if (std.mem.eql(u8, "-parse", args[opt.pos])) {
                if (opt.lex_only) return CLIError.ConflictingFlag;
                opt.parse_only = true;
            } else {
                std.log.err("Unrecognized Compiler Flag {s}", .{args[opt.pos]});
                return CLIError.FlagDoesNotExist;
            }
        }

        return opt;
    }
};

fn usage(writer: anytype) !void {
    try writer.print("Usage: osmium [main_file] [flags]\n", .{});
    try writer.print("  flags:\n", .{});
    try writer.print("  -o [output file]: specify an output file follow this flag with a filename\n", .{});
    try writer.print("  -native: compile to native code\n", .{});
    try writer.print("  -lex: ONLY perform lexical analysis (mainly for compiler debugging purposes)\n", .{});
    try writer.print("  -parse: ONLY generate the AST (mainly for compiler debugging purposes)\n", .{});
    try writer.print("ex: osmium main.os -o hello.exe\n", .{});
}

fn print_lexer(writer: anytype, l: *lexing.Lexer) !void {
    var t = l.next() orelse return;
    while (t.tag != .EOF) : (t = l.next() orelse return) {
        try writer.print("{s}\n", .{t});
    }
}

fn print_parser(writer: anytype, p: *parsing.ParserState) !void {
    for (p.top_level) |decl| {
        try writer.print("{any} \n", .{decl});
    }
}
