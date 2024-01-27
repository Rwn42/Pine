const std = @import("std");
const assert = std.debug.assert;

const Location = @import("../common.zig").Location;

pub const TokenType = union(enum) {
    //tokens with data
    Identifier: []u8,
    String: []u8,
    Integer: i64,
    Float: f64,

    //keywords
    KEYWORD_COUNT_BEGIN, // used to assert that the keywords map is exhaustive
    Fn,
    Record,
    Return,
    True,
    False,
    If,
    Else,
    For,
    Cast,
    Import,
    While,
    TempPrint,
    KEYWORD_COUNT_END, // used to assert that the keywords map is exhaustive

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

    //unary operators
    Hat,
    ExclamationMark,
    Ampersand,

    //punctuation
    Dot,
    DoubleDot,
    Comma,
    Semicolon,
    Colon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    //misc / control
    EOF,

    pub const Keywords = std.ComptimeStringMap(TokenType, .{
        .{ "fn", .Fn },
        .{ "record", .Record },
        .{ "false", .False },
        .{ "true", .True },
        .{ "if", .If },
        .{ "else", .Else },
        .{ "for", .For },
        .{ "while", .While },
        .{ "return", .Return },
        .{ "#cast", .Cast },
        .{ "#import", .Import },
        .{ "#print", .TempPrint },
    });

    pub fn eq(t1: TokenType, t2: TokenType) bool {
        return @intFromEnum(t1) == @intFromEnum(t2);
    }

    pub const Repr = std.ComptimeStringMap([]const u8, .{
        .{ @tagName(TokenType.Fn), "fn" },
        .{ @tagName(TokenType.Record), "record" },
        .{ @tagName(TokenType.True), "true" },
        .{ @tagName(TokenType.False), "false" },
        .{ @tagName(TokenType.If), "if" },
        .{ @tagName(TokenType.Else), "else" },
        .{ @tagName(TokenType.Return), "return" },
        .{ @tagName(TokenType.For), "for" },
        .{ @tagName(TokenType.While), "while" },
        .{ @tagName(TokenType.Plus), "+" },
        .{ @tagName(TokenType.Dash), "-" },
        .{ @tagName(TokenType.SlashForward), "/" },
        .{ @tagName(TokenType.Asterisk), "*" },
        .{ @tagName(TokenType.DoubleEqual), "==" },
        .{ @tagName(TokenType.LessThan), "<" },
        .{ @tagName(TokenType.LessThanEqual), "<=" },
        .{ @tagName(TokenType.GreaterThan), ">" },
        .{ @tagName(TokenType.GreaterThanEqual), ">=" },
        .{ @tagName(TokenType.NotEqual), "!=" },
        .{ @tagName(TokenType.Hat), "^" },
        .{ @tagName(TokenType.ExclamationMark), "!" },
        .{ @tagName(TokenType.Ampersand), "&" },
        .{ @tagName(TokenType.Dot), "." },
        .{ @tagName(TokenType.DoubleDot), ".." },
        .{ @tagName(TokenType.Comma), "," },
        .{ @tagName(TokenType.Semicolon), ";" },
        .{ @tagName(TokenType.Colon), ":" },
        .{ @tagName(TokenType.Lparen), "(" },
        .{ @tagName(TokenType.Rparen), ")" },
        .{ @tagName(TokenType.Lbrace), "{" },
        .{ @tagName(TokenType.Rbrace), "}" },
        .{ @tagName(TokenType.Lbracket), "[" },
        .{ @tagName(TokenType.Rbracket), "]" },
        .{ @tagName(TokenType.EOF), "End of File" },
        .{ @tagName(TokenType.TempPrint), "#print" },
        .{ @tagName(TokenType.Cast), "#cast" },
        .{ @tagName(TokenType.Import), "#import" },
        .{ @tagName(TokenType.Equal), "=" },
    });

    pub fn format(self: TokenType, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        switch (self) {
            .Identifier, .String => try writer.print("Identifier", .{}),
            .Integer => try writer.print("Integer", .{}),
            .Float => try writer.print("Float", .{}),
            else => {
                if (Repr.get(@tagName(self)) == null) {
                    try writer.print("no representation for tag {s}", .{@tagName(self)});
                } else try writer.print("'{s}'", .{Repr.get(@tagName(self)).?});
            },
        }
    }
};

pub const Token = struct {
    loc: Location,
    tag: TokenType,

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self.tag) {
            .Identifier, .String => |val| try writer.print("'{s}'", .{val}),
            .Integer => |val| try writer.print("'{d}'", .{val}),
            .Float => |val| try writer.print("'{d}'", .{val}),
            else => {
                if (TokenType.Repr.get(@tagName(self.tag)) == null) {
                    try writer.print("no representation for tag {s}", .{@tagName(self.tag)});
                } else try writer.print("'{s}'", .{TokenType.Repr.get(@tagName(self.tag)).?});
            },
        }
        try writer.print(" {s}", .{self.loc});
    }
};

//assert keyword map is exhaustive
comptime {
    const keyword_begin = @intFromEnum(TokenType.KEYWORD_COUNT_BEGIN);
    const keyword_end = @intFromEnum(TokenType.KEYWORD_COUNT_END);
    assert((keyword_end - keyword_begin) - 1 == TokenType.Keywords.kvs.len);
}
