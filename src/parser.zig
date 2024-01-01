const std = @import("std");
const assert = @import("std").debug.assert;

const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");

//TODO: support parenthesis
//TODO: Decide on how errors are handled
//TODO: get rid of all the int to enums
//TODO: Move '-' from lexing stage to be a prefix op instead
//TODO: function calls
//TODO: accesses like array.0 or record.x

const Precedence = enum {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,

    fn from(typ: TokenType) usize {
        const prec: Precedence = switch (typ) {
            .DoubleEqual => .Equals,
            .NotEqual => .Equals,
            .LessThan => .LessGreater,
            .GreaterThan => .LessGreater,
            .LessThanEqual => .LessGreater,
            .GreaterThanEqual => .LessGreater,
            .Plus => .Sum,
            .Dash => .Sum,
            .Asterisk => .Product,
            .SlashForward => .Product,
            else => .Lowest,
        };
        return @intFromEnum(prec);
    }
};

const ParseError = error{
    UnexpectedToken,
    LexerError,
};

pub const ParserState = struct {
    const Self = @This();

    lexer: *Lexer,
    token: Token,
    peek_token: Token,
    node_arena: std.heap.ArenaAllocator,

    pub fn init(l: *Lexer, allocator: std.mem.Allocator) ?Self {
        return .{
            .lexer = l,
            .node_arena = std.heap.ArenaAllocator.init(allocator),
            .token = l.next() orelse return null,
            .peek_token = l.next() orelse return null,
        };
    }

    pub fn parse(self: *Self) AST.Expression {
        const expr = ExpressionParser.parse(self) catch {
            @panic("parse erro");
        };
        _ = self.expect(&[_]TokenType{.Semicolon});
        self.adv().?;
        self.adv().?;

        return expr;
    }

    fn adv(self: *Self) ?void {
        self.token = self.peek_token;
        self.peek_token = self.lexer.next() orelse return null;
    }

    //performs expect on peek_token
    fn expect(self: *Self, expected: []const TokenType) bool {
        for (expected) |expTag| {
            if (@intFromEnum(self.peek_token.tag) == @intFromEnum(expTag)) return true;
        }
        std.log.err("Unexpected token {s}", .{self.peek_token});
        return false;
    }

    fn new_node(self: *Self, comptime T: type) *T {
        return self.node_arena.allocator().create(T) catch {
            @panic("COMPILER ERROR: Could not allocate node!");
        };
    }

    pub fn deinit(self: *Self) void {
        self.node_arena.deinit();
    }
};

// Expression parsing based off the building an interpreter in go book.
const ExpressionParser = struct {
    fn parse(p: *ParserState) !AST.Expression {
        return parse_precedence(p, .Lowest);
    }

    fn is_infix_op(typ: TokenType) bool {
        return switch (typ) {
            .Plus, .Dash, .Asterisk, .SlashForward => true,
            else => false,
        };
    }

    fn parse_precedence(p: *ParserState, prec: Precedence) ParseError!AST.Expression {
        var expr: AST.Expression = switch (p.token.tag) {
            .Identifier => .{ .IdentifierUsage = p.token },
            .Integer => .{ .LiteralInt = p.token },
            .Float => .{ .LiteralFloat = p.token },
            .True, .False => .{ .LiteralBool = p.token },
            .String => .{ .LiteralString = p.token },
            .Hat, .ExclamationMark, .Ampersand => try parse_prefix(p),
            else => @panic("expression business"),
        };

        while (p.peek_token.tag != .Semicolon and @intFromEnum(prec) < Precedence.from(p.peek_token.tag)) {
            if (!is_infix_op(p.peek_token.tag)) return expr;
            p.adv() orelse return ParseError.LexerError;
            expr = try parse_infix(p, expr);
        }

        return expr;
    }

    fn parse_prefix(p: *ParserState) !AST.Expression {
        var expr = p.new_node(AST.UnaryExpression);
        expr.op = p.token;

        p.adv() orelse return ParseError.LexerError;

        expr.expr = try parse_precedence(p, .Prefix);
        return .{ .UnaryExprNode = expr };
    }

    fn parse_infix(p: *ParserState, lhs: AST.Expression) !AST.Expression {
        var expr = p.new_node(AST.BinaryExpression);
        expr.lhs = lhs;
        expr.op = p.token;

        const prec = Precedence.from(p.token.tag);
        p.adv() orelse return ParseError.LexerError;
        expr.rhs = try parse_precedence(p, @enumFromInt(prec));

        return .{ .BinaryExprNode = expr };
    }
};
