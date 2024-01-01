const std = @import("std");
const assert = @import("std").debug.assert;

const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");

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
    const ERROR_THRESHOLD = 12; // random choice of value will halt parsing if errors member exceeds

    lexer: *Lexer,
    token: Token,
    peek_token: Token,
    node_arena: std.heap.ArenaAllocator,
    errors: usize,

    pub fn init(l: *Lexer, allocator: std.mem.Allocator) ?Self {
        return .{
            .lexer = l,
            .node_arena = std.heap.ArenaAllocator.init(allocator),
            .token = l.next() orelse return null,
            .peek_token = l.next() orelse return null,
            .errors = 0,
        };
    }

    pub fn parse(self: *Self) AST.Expression {
        return ExpressionParser.parse(self);
    }

    fn adv(self: *Self) !void {
        self.token = self.peek_token;
        self.peek_token = self.lexer.next() orelse return ParseError.LexerError;
    }

    //performs expect on peek_token
    fn expect(self: *Self, expected: []const TokenType) ParseError!void {
        for (expected) |expTag| {
            if (@intFromEnum(self.peek_token.tag) == @intFromEnum(expTag)) return;
        }
        std.log.err("Unexpected token {s}", .{self.peek_token});
        return ParseError.UnexpectedToken;
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
    // does not return an error just increments the global error tracker and returns a dummy value on failure
    // this allows the parser to keep parsing until an error threshold is met.
    fn parse(p: *ParserState) AST.Expression {
        const expr = parse_precedence(p, .Lowest) catch {
            p.errors += 1;
            return dummy_expression(p);
        };

        p.expect(&[_]TokenType{.Semicolon}) catch {
            p.errors += 1;
            return dummy_expression(p);
        };

        p.adv() catch {
            p.errors += 1;
        };
        p.adv() catch {
            p.errors += 1;
        };

        return expr;
    }

    fn is_infix_op(typ: TokenType) bool {
        return switch (typ) {
            .Plus, .Dash, .Asterisk, .SlashForward => true,
            else => false,
        };
    }

    fn parse_precedence(p: *ParserState, prec: Precedence) ParseError!AST.Expression {
        var expr: AST.Expression = switch (p.token.tag) {
            .Integer => .{ .LiteralInt = p.token },
            .Float => .{ .LiteralFloat = p.token },
            .True, .False => .{ .LiteralBool = p.token },
            .String => .{ .LiteralString = p.token },
            .Hat, .ExclamationMark, .Ampersand, .Dash => try parse_prefix(p),
            .Lparen => try parse_grouped(p),
            .Identifier => blk: {
                break :blk switch (p.peek_token.tag) {
                    .Lparen => try parse_call(p),
                    .Dot => @panic("Not Implemented"), //struct / array access
                    else => .{ .IdentifierUsage = p.token },
                };
            },
            else => {
                std.log.err("Cannot start expression with token {s}", .{p.token});
                return ParseError.UnexpectedToken;
            },
        };

        while (p.peek_token.tag != .Semicolon and @intFromEnum(prec) < Precedence.from(p.peek_token.tag)) {
            if (!is_infix_op(p.peek_token.tag)) return expr;
            try p.adv();
            expr = try parse_infix(p, expr);
        }

        return expr;
    }

    fn parse_prefix(p: *ParserState) !AST.Expression {
        var expr = p.new_node(AST.UnaryExpression);
        expr.op = p.token;

        try p.adv();

        expr.expr = try parse_precedence(p, .Prefix);
        return .{ .UnaryExprNode = expr };
    }

    fn parse_infix(p: *ParserState, lhs: AST.Expression) !AST.Expression {
        var expr = p.new_node(AST.BinaryExpression);
        expr.lhs = lhs;
        expr.op = p.token;

        const prec = Precedence.from(p.token.tag);
        try p.adv();
        expr.rhs = try parse_precedence(p, @enumFromInt(prec));

        return .{ .BinaryExprNode = expr };
    }

    fn parse_grouped(p: *ParserState) !AST.Expression {
        try p.adv(); //consume lparen

        var expr = try parse_precedence(p, .Lowest);

        try p.expect(&[_]TokenType{.Rparen});
        try p.adv();

        return expr;
    }

    fn parse_call(p: *ParserState) !AST.Expression {
        var expr = p.new_node(AST.FunctionCallExpression);
        expr.name = p.token;
        expr.args_list = null;

        try p.adv(); //consume lparen
        try p.adv();

        var prev_ptr: *AST.ExprList = undefined;

        if (p.token.tag != .Rparen) {
            const arg = try parse_precedence(p, .Lowest);
            const new_node = p.new_node(AST.ExprList);
            new_node.next = null;
            new_node.expr = arg;
            expr.args_list = new_node;
            prev_ptr = new_node;
            try p.expect(&[_]TokenType{ .Comma, .Rparen });
        } else {
            return .{ .FuncCall = expr };
        }

        if (p.peek_token.tag == .Comma) {
            try p.adv();
        }

        while (p.peek_token.tag != .Rparen) {
            try p.adv();
            const arg = try parse_precedence(p, .Lowest);
            const new_node = p.new_node(AST.ExprList);
            new_node.next = null;
            new_node.expr = arg;
            prev_ptr.next = new_node;
            prev_ptr = new_node;
            try p.expect(&[_]TokenType{ .Comma, .Rparen });
            if (p.peek_token.tag == .Comma) try p.adv();
        }

        try p.adv(); //consume rparen

        return .{ .FuncCall = expr };
    }

    fn dummy_expression(p: *ParserState) AST.Expression {
        return .{ .IdentifierUsage = p.token };
    }
};
