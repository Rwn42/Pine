const std = @import("std");

const lexing = @import("lexer.zig");
const parsing = @import("parser.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const StringManager = @import("common.zig").StringManager;

//TODO: Add more to CLI.

const MAX_FILE_BYTES = 1024 * 1024;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const cli_options = CLIOptions.create(args) catch |err| {
        switch (err) {
            CLIOptions.CLIError.NoInput => std.log.err("No input file provided", .{}),
            CLIOptions.CLIError.NoOutput => std.log.err("No output file provided after -o", .{}),
        }
        return;
    };

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

    var l = lexing.Lexer.init(file_buffer, cli_options.input_file, &sm) orelse return;
    var p = parsing.ParserState.init(&l, allocator) orelse return;
    const exp = p.parse();
    defer p.deinit();
    std.debug.print("{any} \n", .{exp});
}

//wanted to use argIterator here but i couldnt get it to work
const CLIOptions = struct {
    input_file: []const u8,
    output_file: ?([]const u8),
    pos: usize,

    pub const CLIError = error{
        NoInput,
        NoOutput,
    };

    pub fn create(args: [][]u8) !CLIOptions {
        var opt = CLIOptions{
            .input_file = undefined,
            .output_file = null,
            .pos = 0,
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
            }
        }

        return opt;
    }
};
