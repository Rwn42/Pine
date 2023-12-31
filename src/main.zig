const std = @import("std");

const lexing = @import("lexer.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const StringManager = @import("common.zig").StringManager;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer {
        // _ = gpa.detectLeaks();
        _ = gpa.deinit();
    }

    const filepath = "test.os";

    const file_buffer = std.fs.cwd().readFileAlloc(allocator, filepath, 1024 * 1024) catch |err| {
        switch (err) {
            error.FileTooBig => std.log.err("File {s} is to big try splitting your program into more files.", .{filepath}),
            else => std.log.err("File {s} was not found.", .{filepath}),
        }
        return;
    };

    defer allocator.free(file_buffer);

    if (file_buffer.len == 0) {
        std.log.err("File {s} was found but is empty", .{filepath});
        return;
    }

    var sm = StringManager.init(allocator);
    defer sm.destroy();

    var l = lexing.Lexer.init(file_buffer, filepath, &sm) orelse return;
    var t = l.next() orelse return;
    std.debug.print("\n", .{});
    while (t.tag != .EOF) : (t = l.next() orelse return) {
        std.debug.print("{s} ", .{t});
        switch (t.tag) {
            //.Identifier, .String => |val| std.debug.print("found text {d} \n", .{}),
            else => std.debug.print("\n", .{}),
        }
    }
}
