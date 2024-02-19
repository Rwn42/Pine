const std = @import("std");

pub const FileLocation = struct {
    row: usize,
    col: usize,
    filename: []const u8,

    pub fn format(self: FileLocation, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}:{d}:{d}", .{ self.filename, self.row, self.col });
    }
};

pub const TokenTag = union(enum) {
    //tokens with data
    Identifier: []const u8,
    String: []const u8,
    Integer: i64,
    Float: f64,

    Fn,
    Record,
    Return,
    Pub,
    True,
    False,
    If,
    Else,
    Cast,
    Import,
    While,
    Foreign,

    // Binary operators
    Plus,
    Dash,
    SlashForward,
    Asterisk,
    DoubleEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    NotEqual,
    Equal,

    //punctuation
    Dot,
    DoubleDot,
    Comma,
    Colon,
    DoubleColon,
    Hat,
    ExclamationMark,
    Ampersand,

    //brackets
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    //misc / control
    EOF,
    Newline,

    pub const Keywords = std.ComptimeStringMap(TokenTag, .{
        .{ "fn", .Fn },
        .{ "pub", .Pub },
        .{ "record", .Record },
        .{ "false", .False },
        .{ "true", .True },
        .{ "if", .If },
        .{ "else", .Else },
        .{ "while", .While },
        .{ "return", .Return },
        .{ "#cast", .Cast },
        .{ "#import", .Import },
        .{ "#foreign", .Foreign },
    });

    pub fn eq(t1: TokenTag, t2: TokenTag) bool {
        return @intFromEnum(t1) == @intFromEnum(t2);
    }

    pub fn format(self: TokenTag, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        switch (self) {
            .Identifier => |val| try writer.print("Identifier {s}", .{val}),
            .String => |val| try writer.print("String \"{s}\"", .{val}),
            .Integer => |num| try writer.print("Untyped Integer {d}", .{num}),
            .Float => |num| try writer.print("Float {d}", .{num}),
            else => {
                if (TagToStr.get(@tagName(self))) |repr| {
                    try writer.print("{s}", .{repr});
                } else {
                    std.log.info("{s}", .{@tagName(self)});
                    @panic("Compiler Error: Token Does Not Have Representation");
                }
            },
        }
    }
};

pub const Token = struct {
    location: FileLocation,
    tag: TokenTag,
    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("{s} at {s}", .{ self.tag, self.location });
    }
};

pub const TagToStr = std.ComptimeStringMap([]const u8, .{
    .{ @tagName(TokenTag.Fn), "fn" },
    .{ @tagName(TokenTag.Record), "record" },
    .{ @tagName(TokenTag.True), "true" },
    .{ @tagName(TokenTag.False), "false" },
    .{ @tagName(TokenTag.If), "if" },
    .{ @tagName(TokenTag.Else), "else" },
    .{ @tagName(TokenTag.Return), "return" },
    .{ @tagName(TokenTag.While), "while" },
    .{ @tagName(TokenTag.Pub), "pub" },
    .{ @tagName(TokenTag.Plus), "+" },
    .{ @tagName(TokenTag.Dash), "-" },
    .{ @tagName(TokenTag.SlashForward), "/" },
    .{ @tagName(TokenTag.Asterisk), "*" },
    .{ @tagName(TokenTag.DoubleEqual), "==" },
    .{ @tagName(TokenTag.LessThan), "<" },
    .{ @tagName(TokenTag.LessThanEqual), "<=" },
    .{ @tagName(TokenTag.GreaterThan), ">" },
    .{ @tagName(TokenTag.GreaterThanEqual), ">=" },
    .{ @tagName(TokenTag.NotEqual), "!=" },
    .{ @tagName(TokenTag.Equal), "=" },
    .{ @tagName(TokenTag.Hat), "^" },
    .{ @tagName(TokenTag.ExclamationMark), "!" },
    .{ @tagName(TokenTag.Ampersand), "&" },
    .{ @tagName(TokenTag.Dot), "." },
    .{ @tagName(TokenTag.DoubleDot), ".." },
    .{ @tagName(TokenTag.Comma), "," },
    .{ @tagName(TokenTag.Colon), ":" },
    .{ @tagName(TokenTag.DoubleColon), "::" },
    .{ @tagName(TokenTag.Lparen), "(" },
    .{ @tagName(TokenTag.Rparen), ")" },
    .{ @tagName(TokenTag.Lbrace), "{" },
    .{ @tagName(TokenTag.Rbrace), "}" },
    .{ @tagName(TokenTag.Lbracket), "[" },
    .{ @tagName(TokenTag.Rbracket), "]" },
    .{ @tagName(TokenTag.EOF), "End of File" },
    .{ @tagName(TokenTag.Newline), "Newline" },
    .{ @tagName(TokenTag.Foreign), "#foreign" },
    .{ @tagName(TokenTag.Cast), "#cast" },
    .{ @tagName(TokenTag.Import), "#import" },
});
