const std = @import("std");
const assert = @import("std").debug.assert;

const Location = @import("../common.zig").Location;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");

const ParseError = error{
    UnexpectedToken,
    LexerError,
    NotEnoughData,
};

//TODO: skip failed parsing steps better (moving on to next token usually results in more errors anway)

pub const ParserState = struct {
    const Self = @This();

    lexer: *Lexer,
    token: Token,
    peek_token: Token,
    node_arena: std.heap.ArenaAllocator,

    top_level: []AST.Declaration,

    pub fn init(l: *Lexer, allocator: std.mem.Allocator) ?Self {
        return .{
            .lexer = l,
            .node_arena = std.heap.ArenaAllocator.init(allocator),
            .token = l.next() orelse return null,
            .peek_token = l.next() orelse return null,
            .top_level = undefined,
        };
    }

    //fills the top level member with all "roots" of the AST
    pub fn parse(self: *Self) void {
        var top_level_declarations = std.ArrayList(AST.Declaration).init(self.node_arena.allocator());
        while (self.token.tag != .EOF) {
            const decl = DeclarationParser.parse(self) catch {
                continue; //if the declaration failed skip it try and parse the next one
            };
            top_level_declarations.append(decl) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };
        }

        assert(self.token.tag == .EOF);

        self.top_level = top_level_declarations.toOwnedSlice() catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    fn adv(self: *Self) !void {
        self.token = self.peek_token;
        self.peek_token = self.lexer.next() orelse return ParseError.LexerError;
    }

    fn expect_delimiter(self: *Self, expected: TokenType) !void {
        if (!(TokenType.eq(self.peek_token.tag, expected))) {
            std.log.err("Unexpected token {s} expected {s}", .{ self.peek_token, expected });
            try self.adv();
            try self.adv();
            return ParseError.UnexpectedToken;
        }
        try self.adv();
        try self.adv();
    }

    fn assert_token_is(self: *Self, expected: TokenType) !Token {
        if (!(TokenType.eq(self.token.tag, expected))) {
            std.log.err("Unexpected token {s} expected {s}", .{ self.token, expected });
            try self.adv();
            return ParseError.UnexpectedToken;
        }
        const v = self.token;
        try self.adv();
        return v;
    }

    fn expect(self: *Self, expected: TokenType) !Token {
        if (!(TokenType.eq(self.peek_token.tag, expected))) {
            std.log.err("Unexpected token {s} expected {s}", .{ self.peek_token, expected });
            try self.adv();
            return ParseError.UnexpectedToken;
        }
        const v = self.peek_token;
        try self.adv();
        return v;
    }

    fn new_node(self: *Self, comptime T: type) *T {
        return self.node_arena.allocator().create(T) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    pub fn deinit(self: *Self) void {
        self.node_arena.deinit();
    }
};

pub const DeclarationParser = struct {
    fn parse(p: *ParserState) !AST.Declaration {
        const name_tk = try p.assert_token_is(.{ .Identifier = "" });
        _ = try p.assert_token_is(.Colon);
        _ = try p.assert_token_is(.Colon);

        switch (p.token.tag) {
            .Fn => return try parse_func_decl(p, name_tk),
            .Record => return try parse_record_decl(p, name_tk),
            else => return try parse_const_decl(p, name_tk),
        }
    }

    fn parse_record_decl(p: *ParserState, name_tk: Token) !AST.Declaration {
        const decl = p.new_node(AST.RecordDeclarationNode);
        decl.name_tk = name_tk;

        _ = try p.expect_delimiter(.Lbrace);
        try parse_param_list(p, &decl.fields);
        _ = try p.assert_token_is(.Rbrace);

        if (decl.fields == null) {
            std.log.err("Record must contain atleast one field {s}", .{name_tk});
            return ParseError.NotEnoughData;
        }

        return .{ .RecordDeclaration = decl };
    }

    fn parse_const_decl(p: *ParserState, name_tk: Token) !AST.Declaration {
        const decl = p.new_node(AST.ConstantDeclarationNode);
        decl.name_tk = name_tk;
        decl.value = try ExpressionParser.parse(p, .Semicolon);
        return .{ .ConstantDeclaration = decl };
    }

    fn parse_func_decl(p: *ParserState, name_tk: Token) !AST.Declaration {
        var decl = p.new_node(AST.FunctionDeclarationNode);
        var body = std.ArrayList(AST.Statement).init(p.node_arena.allocator());

        decl.name_tk = name_tk;
        decl.params = null;
        decl.return_typ = undefined;

        try p.expect_delimiter(.Lparen);

        if (!TokenType.eq(p.token.tag, .Rparen)) {
            try parse_param_list(p, &decl.params);
            try p.adv();
        } else {
            try p.adv();
        }

        decl.return_typ = try TypeParser.parse(p);

        _ = try p.assert_token_is(.Lbrace);

        while (!TokenType.eq(p.token.tag, TokenType.Rbrace)) {
            body.append(try StatementParser.parse(p)) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };
        }

        _ = try p.assert_token_is(.Rbrace);

        decl.body = body.toOwnedSlice() catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
        return .{ .FunctionDeclaration = decl };
    }

    fn parse_param_list(p: *ParserState, prev: *?*AST.ParamList) !void {
        var param = p.new_node(AST.ParamList);
        param.next = null;

        param.name_tk = try p.assert_token_is(.{ .Identifier = "" });
        _ = try p.assert_token_is(.Colon);

        param.typ = try TypeParser.parse(p);

        prev.* = param;

        if (TokenType.eq(p.token.tag, .Rparen) or TokenType.eq(p.token.tag, .Rbrace)) return;
        _ = try p.assert_token_is(.Comma);
        if (TokenType.eq(p.token.tag, .Rparen) or TokenType.eq(p.token.tag, .Rbrace)) return; //for trailing commmas

        return parse_param_list(p, &param.next);
    }
};

pub const TypeParser = struct {
    fn parse(p: *ParserState) !AST.DefinedType {
        const initial = p.token;
        try p.adv();
        switch (initial.tag) {
            .Identifier => return .{ .Basic = initial },
            .Hat => {
                const new_node = p.new_node(AST.PointerType);
                new_node.pointing_to = try parse(p);
                return .{ .Pointer = new_node };
            },
            .Lbracket => {
                const new_node = p.new_node(AST.ArrayType);
                const length = p.token;
                if (!(TokenType.eq(length.tag, .{ .Integer = 0 })) and !(TokenType.eq(length.tag, .{ .Identifier = "" }))) {
                    std.log.err("array length must be an integer or constant value got {s}", .{p.token});
                    return ParseError.UnexpectedToken;
                }
                try p.expect_delimiter(.Rbracket);

                new_node.length = length;
                new_node.element_typ = try parse(p);

                return .{ .Array = new_node };
            },
            else => {
                std.log.info("Expected a type got {s}", .{initial});
                return ParseError.UnexpectedToken;
            },
        }
    }
};

const StatementParser = struct {
    fn parse(p: *ParserState) !AST.Statement {
        const initial = p.token;
        switch (initial.tag) {
            .Return => {
                try p.adv();
                return .{ .ReturnStatement = try ExpressionParser.parse(p, .Semicolon) };
            },
            .Identifier => {
                switch (p.peek_token.tag) {
                    .Dot, .Equal => {
                        return try parse_assignment(p);
                    },
                    .Colon => {
                        try p.adv();
                        _ = try p.assert_token_is(.Colon);
                        return try parse_var_decl(p, initial);
                    },
                    else => return .{ .ExpressionStatement = try ExpressionParser.parse(p, .Semicolon) },
                }
            },
            .If => {
                try p.adv();
                return try parse_conditional(p, false, initial.loc);
            },
            .While => {
                try p.adv();
                return try parse_conditional(p, true, initial.loc);
            },
            .Hat => {
                return try parse_assignment(p);
            },
            else => {
                std.log.err("Cannot begin statement with {t}", .{initial});
                return ParseError.UnexpectedToken;
            },
        }
    }

    fn parse_assignment(p: *ParserState) !AST.Statement {
        const new_node = p.new_node(AST.VariableAssignmentNode);
        new_node.lhs = try ExpressionParser.parse(p, .Equal);
        new_node.rhs = try ExpressionParser.parse(p, .Semicolon);
        return .{ .VariableAssignment = new_node };
    }

    fn parse_var_decl(p: *ParserState, name_tk: Token) !AST.Statement {
        var node = p.new_node(AST.VariableDeclarationNode);
        node.name_tk = name_tk;
        node.assignment = null;
        node.typ = null;

        if (TokenType.eq(p.token.tag, .Equal)) { // x := <expr>
            try p.adv();
            node.assignment = try ExpressionParser.parse(p, .Semicolon);
        } else { // x: <type>
            node.typ = try TypeParser.parse(p);
            if (TokenType.eq(p.token.tag, .Equal)) { // x : <type> = <expr>
                try p.adv();
                node.assignment = try ExpressionParser.parse(p, .Semicolon);
            } else { //x : <type>;
                _ = try p.assert_token_is(.Semicolon);
            }
        }

        return .{ .VariableDeclaration = node };
    }

    fn parse_conditional(p: *ParserState, is_while: bool, start_loc: Location) ParseError!AST.Statement {
        const condition = try ExpressionParser.parse(p, .Lbrace);

        var body = std.ArrayList(AST.Statement).init(p.node_arena.allocator());

        while (!TokenType.eq(p.token.tag, .Rbrace)) {
            const s = try StatementParser.parse(p);
            body.append(s) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };
        }
        _ = try p.assert_token_is(.Rbrace);

        if (is_while) {
            var node = p.new_node(AST.WhileStatementNode);
            node.start_loc = start_loc;
            node.condition = condition;
            node.body = body.toOwnedSlice() catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };
            return .{ .WhileStatement = node };
        }

        var node = p.new_node(AST.IfStatementNode);
        node.start_loc = start_loc;
        node.condition = condition;
        node.body = body.toOwnedSlice() catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };

        return .{ .IfStatement = node };
    }
};

// Most of expression parsing based off the building an interpreter in go book.
const ExpressionParser = struct {
    fn parse(p: *ParserState, end: TokenType) !AST.Expression {
        const expr = try parse_precedence(p, .Lowest);
        try p.expect_delimiter(end);
        return expr;
    }

    fn is_infix_op(typ: TokenType) bool {
        return switch (typ) {
            .Plus,
            .Dash,
            .Asterisk,
            .SlashForward,
            .LessThan,
            .LessThanEqual,
            .GreaterThan,
            .GreaterThanEqual,
            .DoubleEqual,
            .NotEqual,
            .Dot,
            => true,
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
            .Lbracket => blk: {
                try p.adv();
                var v: ?*AST.ExprList = null;
                try parse_arg(p, &v, .Rbracket);
                _ = try p.expect(.Rbracket);
                if (v) |node| {
                    break :blk .{ .ArrayInitialization = node };
                }
                std.log.err("Array intialization must contain atleast one value near {s}", .{p.token});
                return ParseError.UnexpectedToken;
            },
            .Identifier => blk: {
                break :blk switch (p.peek_token.tag) {
                    .Lparen => try parse_call(p),
                    .Lbrace => try parse_record(p),
                    else => .{ .IdentifierInvokation = p.token },
                };
            },
            else => {
                std.log.err("Cannot start expression with token {s}", .{p.token});
                return ParseError.UnexpectedToken;
            },
        };
        while (!TokenType.eq(p.peek_token.tag, .Semicolon) and @intFromEnum(prec) < Precedence.from(p.peek_token.tag)) {
            if (!is_infix_op(p.peek_token.tag)) return expr;
            try p.adv();
            expr = try parse_infix(p, expr);
        }

        return expr;
    }

    fn parse_prefix(p: *ParserState) !AST.Expression {
        var expr = p.new_node(AST.UnaryExpressionNode);
        expr.op = p.token;

        try p.adv();

        expr.expr = try parse_precedence(p, .Prefix);
        return .{ .UnaryExpression = expr };
    }

    fn parse_infix(p: *ParserState, lhs: AST.Expression) !AST.Expression {
        var expr = p.new_node(AST.BinaryExpressionNode);
        expr.lhs = lhs;
        expr.op = p.token;

        const prec = Precedence.from(p.token.tag);
        try p.adv();

        //dot is right associative subtracting 1 from the precedence seems to work for that
        if (TokenType.eq(expr.op.tag, .Dot)) {
            expr.rhs = try parse_precedence(p, @enumFromInt(prec - 1));
        } else {
            expr.rhs = try parse_precedence(p, @enumFromInt(prec));
        }

        if (TokenType.eq(expr.op.tag, .Dot)) {
            return .{ .AccessExpression = expr };
        }

        return .{ .BinaryExpression = expr };
    }

    //group expressions are those in parenthesis
    fn parse_grouped(p: *ParserState) !AST.Expression {
        try p.adv(); //consume lparen

        var expr = try parse_precedence(p, .Lowest);

        _ = try p.expect(.Rparen);

        return expr;
    }

    fn parse_call(p: *ParserState) !AST.Expression {
        var expr = p.new_node(AST.FunctionInvokationNode);
        expr.name_tk = p.token;
        expr.args_list = null;

        try p.adv(); //lparen cur token
        try p.adv(); //first argument / rparen is cur token

        if (TokenType.eq(p.token.tag, .Rparen)) return .{ .FunctionInvokation = expr };

        try parse_arg(p, &expr.args_list, .Rparen);
        try p.adv(); //consume rparen

        return .{ .FunctionInvokation = expr };
    }

    fn parse_arg(p: *ParserState, prev: *?*AST.ExprList, end: TokenType) !void {
        const arg = try parse_precedence(p, .Lowest);
        const new_node = p.new_node(AST.ExprList);

        new_node.next = null;
        new_node.expr = arg;
        prev.* = new_node;

        if (TokenType.eq(p.peek_token.tag, end)) return;

        try p.expect_delimiter(.Comma);
        return parse_arg(p, &new_node.next, end);
    }

    fn parse_record(p: *ParserState) !AST.Expression {
        const name = try p.assert_token_is(.{ .Identifier = "" });
        var v: ?*AST.FieldList = null;
        try p.adv(); //consume lbrace
        try parse_field(p, &v);
        try p.adv(); //consume rbrace

        const new_node = p.new_node(AST.RecordInitializationNode);
        new_node.name_tk = name;
        if (v) |node| {
            new_node.fields = node;
            return .{ .RecordInitialization = new_node };
        }

        std.log.err("record initialization must initialize fields near {s}", .{p.token});
        return ParseError.UnexpectedToken;
    }

    fn parse_field(p: *ParserState, prev: *?*AST.FieldList) !void {
        const new_node = p.new_node(AST.FieldList);

        new_node.next = null;
        new_node.field = try p.assert_token_is(.{ .Identifier = "" });
        _ = try p.assert_token_is(.Colon);
        new_node.expr = try parse_precedence(p, .Lowest);
        prev.* = new_node;

        if (TokenType.eq(p.peek_token.tag, .Rbrace)) return;
        try p.expect_delimiter(.Comma);
        return parse_field(p, &new_node.next);
    }
};

const Precedence = enum {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Highest,

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
            .Dot => .Highest,
            else => .Lowest,
        };
        return @intFromEnum(prec);
    }
};
