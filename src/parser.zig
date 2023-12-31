const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");

//TODO: Implement unary operator parsing
//TODO: code cleanup
//TODO: support parenthesis

pub const Parser = struct {
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

    pub fn parse(self: *Self) ?AST.Expression {
        return ExpressionParser.parse(self);
    }

    fn adv(self: *Self) ?void {
        self.token = self.peek_token;
        self.peek_token = self.lexer.next() orelse return null;
    }

    //performs expect on peek_token
    fn expect(self: *Self, expected: []TokenType) bool {
        for (expected) |expTag| {
            if (self.peek_token.tag == expTag) return true;
        }
        return false;
    }

    //expects a sequence of tokens in order will advance the lexer
    fn expect_sequence(self: *Self, expected_seq: []TokenType) bool {
        for (expected_seq) |expTag| {
            if (self.peek_token.tag != expTag) return false;
            self.adv() orelse return false;
        }
    }

    pub fn deinit(self: *Self) void {
        self.node_arena.deinit();
    }
};

//expression parser implements precedence climbing
const ExpressionParser = struct {
    fn precedence(typ: TokenType) u8 {
        return switch (typ) {
            .Plus, .Dash => 1,
            .Asterisk, .SlashForward => 2,
            .Hat, .Ampersand => 3,
            else => {
                std.log.info("{any}", .{typ});
                @panic("COMPILER ERROR: precedence not defined");
            },
        };
    }

    fn is_bin_op(typ: TokenType) bool {
        return switch (typ) {
            .Plus, .Dash, .Asterisk, .SlashForward => true,
            else => false,
        };
    }

    fn is_un_op(typ: TokenType) bool {
        return switch (typ) {
            .Hat, .Ampersand, .ExclamationMark => true,
            else => false,
        };
    }

    fn parse(parser: *Parser) ?AST.Expression {
        const lhs = parse_primary(parser) orelse return null;
        return parse_precedence(parser, lhs, 0);
    }

    fn parse_primary(parser: *Parser) ?AST.Expression {
        const expr: AST.Expression = switch (parser.token.tag) {
            .Identifier => .{ .IdentifierUsage = parser.token }, //eventually will check for func call
            .Integer => .{ .LiteralInt = parser.token },
            .Float => .{ .LiteralFloat = parser.token },
            .True, .False => .{ .LiteralBool = parser.token },
            .String => .{ .LiteralString = parser.token },
            else => {
                std.log.err("Cannot start expression with {t}", .{parser.token});
                return null;
            },
        };
        return expr;
    }

    fn parse_precedence(p: *Parser, lhs: AST.Expression, min_precedence: u8) ?AST.Expression {
        var result: AST.Expression = lhs;
        while (p.peek_token.tag != .Semicolon and is_bin_op(p.peek_token.tag) and precedence(p.peek_token.tag) >= min_precedence) {
            const op = p.peek_token;
            p.adv() orelse return null;
            p.adv() orelse return null;
            var rhs = parse_primary(p) orelse return null;
            while (is_bin_op(p.peek_token.tag) and precedence(p.peek_token.tag) > precedence(op.tag)) {
                rhs = parse_precedence(p, rhs, precedence(op.tag) + 1) orelse return null;
                p.adv() orelse return null;
            }
            var resultNode = p.node_arena.allocator().create(AST.BinaryExpression) catch return null;
            resultNode.lhs = result;
            resultNode.rhs = rhs;
            resultNode.op = op;
            result = .{ .BinaryExprNode = resultNode };
        }
        return result;
    }
};
