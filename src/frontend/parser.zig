const std = @import("std");

const ast = @import("ast.zig");
const lexing = @import("lexer.zig");

const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const FileLocation = @import("token.zig").FileLocation;

const ParseError = error{
    OutOfMemory,
    UnexpectedToken,
    UnexpectedEOF,
    LexerError,
    NotEnoughData,
    EOF,
};

pub fn ast_from_file(parser: *ParserState) !ast.AST {
    const allocator = parser.node_arena.allocator();

    var function_buffer = std.ArrayList(*ast.FunctionDeclarationNode).init(allocator);
    var record_buffer = std.ArrayList(*ast.RecordDeclarationNode).init(allocator);
    var import_buffer = std.ArrayList(Token).init(allocator);
    var constant_buffer = std.ArrayList(*ast.ConstantDeclarationNode).init(allocator);
    var foreign_buffer = std.ArrayList(*ast.ForeignDeclarationNode).init(allocator);

    loop: while (parser.token.tag != .EOF) {
        const decl = DeclarationParser.parse(parser) catch |err| {
            switch (err) {
                error.EOF => break :loop,
                else => return err,
            }
        };
        switch (decl) {
            .ImportDeclaration => |token| try import_buffer.append(token),
            .ForeignDeclaration => |foreign| try foreign_buffer.append(foreign),
            .FunctionDeclaration => |function| try function_buffer.append(function),
            .RecordDeclaration => |record| try record_buffer.append(record),
            .ConstantDeclaration => |constant| try constant_buffer.append(constant),
        }
    }

    return ast.AST{
        .constants = try constant_buffer.toOwnedSlice(),
        .functions = try function_buffer.toOwnedSlice(),
        .records = try record_buffer.toOwnedSlice(),
        .imports = try import_buffer.toOwnedSlice(),
        .foreign = try foreign_buffer.toOwnedSlice(),
    };
}

pub const ParserState = struct {
    const Self = @This();

    lexer: *lexing.Lexer,
    token: Token,
    peek_token: Token,
    node_arena: std.heap.ArenaAllocator,
    errored: bool = false,

    pub fn init(l: *lexing.Lexer, allocator: std.mem.Allocator) ?Self {
        return .{
            .lexer = l,
            .node_arena = std.heap.ArenaAllocator.init(allocator),
            .token = l.next() orelse return null,
            .peek_token = l.next() orelse return null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.node_arena.deinit();
    }

    fn adv(self: *Self) !void {
        self.token = self.peek_token;
        self.peek_token = self.lexer.next() orelse return ParseError.LexerError;
    }

    // a.b  -- expects delimeter (.) skips past it so token is now (b)
    fn expect_delimiter(self: *Self, expected: TokenTag) !void {
        if (!(TokenTag.eq(self.peek_token.tag, expected))) {
            std.log.err("Unexpected token {s} expected {s}", .{ self.peek_token, expected });
            try self.adv();
            try self.adv();
            return ParseError.UnexpectedToken;
        }
        try self.adv();
        try self.adv();
    }

    //a -- expects (a) and returns it
    fn assert_token_is(self: *Self, expected: TokenTag) !Token {
        if (!(TokenTag.eq(self.token.tag, expected))) {
            std.log.err("Unexpected token {s} expected {s}", .{ self.token, expected });
            try self.adv();
            return ParseError.UnexpectedToken;
        }
        const v = self.token;
        try self.adv();
        return v;
    }

    //a[] -- expects ([) and returns it skips to it not past like delimeter
    fn expect(self: *Self, expected: TokenTag) !Token {
        if (!(TokenTag.eq(self.peek_token.tag, expected))) {
            std.log.err("Unexpected token {s} expected {s}", .{ self.peek_token, expected });
            try self.adv();
            return ParseError.UnexpectedToken;
        }
        const v = self.peek_token;
        try self.adv();
        return v;
    }

    //used for skipping code with syntax errors hopefully to the next valid sequence
    //if cannot skip to next line (EOF) then fail and return null
    fn skip_line(self: *Self) ?void {
        self.adv() catch return null;
        if (self.token.tag == .EOF) return null;
        if (self.token.tag == .Newline) {
            self.adv() catch return null;
            return;
        }
        return self.skip_line();
    }

    fn new_node(self: *Self, comptime T: type) *T {
        return self.node_arena.allocator().create(T) catch {
            @panic("Out of memory!");
        };
    }
};

pub const TypeParser = struct {
    fn parse(p: *ParserState) !ast.DefinedType {
        const initial = p.token;
        try p.adv();
        switch (initial.tag) {
            //record or primitives like int, Vector2 ect
            .Identifier => return .{ .Basic = initial },
            //pointer types &int, &Vector2
            .Ampersand => {
                const new_node = p.new_node(ast.PointerType);
                new_node.pointing_to = try parse(p);
                return .{ .Pointer = new_node };
            },
            .Lbracket => {
                //wide pointer []type
                if (p.token.tag == .Rbracket) {
                    try p.adv();
                    const new_node = p.new_node(ast.PointerType);
                    new_node.pointing_to = try parse(p);
                    return .{ .WidePointer = new_node };
                }
                //array type [...]type
                const new_node = p.new_node(ast.ArrayType);
                const length = try ExpressionParser.parse(p, .Rbracket);
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

const DeclarationParser = struct {
    fn parse(p: *ParserState) !ast.Declaration {
        const initial = p.token;
        _ = try p.adv();

        return switch (initial.tag) {
            .Import => .{ .ImportDeclaration = try p.assert_token_is(.{ .String = "" }) },
            .Foreign => try parse_foreign(p),
            .Identifier => blk: {
                _ = try p.assert_token_is(.DoubleColon);
                switch (p.token.tag) {
                    .Record => break :blk try parse_record_decl(p, initial),
                    .Fn => break :blk try parse_func_decl(p, initial),
                    else => break :blk try parse_const_decl(p, initial),
                }
            },
            .Newline => try parse(p),
            .EOF => return ParseError.EOF,
            else => {
                std.log.err("No declaration starts with {s}", .{initial});
                return ParseError.UnexpectedToken;
            },
        };
    }

    fn parse_foreign(p: *ParserState) !ast.Declaration {
        var node = p.new_node(ast.ForeignDeclarationNode);
        const library = try p.assert_token_is(.{ .String = "" });
        var imports = std.ArrayList(Token).init(p.node_arena.allocator());
        defer imports.deinit();

        _ = try p.assert_token_is(.Lbracket);
        while (true) {
            try imports.append(try p.assert_token_is(.{ .String = "" }));
            if (p.token.tag == .Rbracket) break;
            _ = try p.assert_token_is(.Comma);
        }
        _ = try p.adv();

        node.library = library;
        node.function_imports = try imports.toOwnedSlice();
        return .{ .ForeignDeclaration = node };
    }

    fn parse_record_decl(p: *ParserState, name_tk: Token) !ast.Declaration {
        const decl = p.new_node(ast.RecordDeclarationNode);
        var fields = std.ArrayList(ast.Param).init(p.node_arena.allocator());
        decl.name_tk = name_tk;

        _ = try p.expect_delimiter(.Lbrace);

        var ll_head: ?*ast.ParamList = null;
        try parse_param_list(p, &ll_head);
        while (ll_head) |node| {
            try fields.append(.{ .name_tk = node.name_tk, .typ = node.typ });
            ll_head = node.next;
        }
        decl.fields = try fields.toOwnedSlice();
        if (decl.fields.len <= 0) {
            std.log.err("Record must contain atleast one field {s}", .{name_tk});
            return ParseError.NotEnoughData;
        }

        _ = try p.assert_token_is(.Rbrace);

        return .{ .RecordDeclaration = decl };
    }

    fn parse_const_decl(p: *ParserState, name_tk: Token) !ast.Declaration {
        const decl = p.new_node(ast.ConstantDeclarationNode);
        decl.name_tk = name_tk;
        decl.value = try ExpressionParser.parse(p, .Newline);
        return .{ .ConstantDeclaration = decl };
    }

    fn parse_func_decl(p: *ParserState, name_tk: Token) !ast.Declaration {
        var decl = p.new_node(ast.FunctionDeclarationNode);
        var body = std.ArrayList(ast.Statement).init(p.node_arena.allocator());
        var params = std.ArrayList(ast.Param).init(p.node_arena.allocator());

        decl.name_tk = name_tk;
        decl.params = undefined;
        decl.return_typ = null;

        try p.expect_delimiter(.Lparen);

        if (!TokenTag.eq(p.token.tag, .Rparen)) {
            var ll_head: ?*ast.ParamList = null;
            try parse_param_list(p, &ll_head);
            while (ll_head) |node| {
                try params.append(.{ .name_tk = node.name_tk, .typ = node.typ });
                ll_head = node.next;
            }
        }

        try p.adv();

        decl.params = try params.toOwnedSlice();

        if (!TokenTag.eq(p.token.tag, .Lbrace)) {
            decl.return_typ = try TypeParser.parse(p);
        }

        _ = try p.assert_token_is(.Lbrace);

        while (!TokenTag.eq(p.token.tag, TokenTag.Rbrace)) {
            const stmt = StatementParser.parse(p) catch {
                p.skip_line() orelse break;
                p.errored = true;
                continue;
            };
            try body.append(stmt);
        }

        _ = try p.assert_token_is(.Rbrace);

        decl.body = try body.toOwnedSlice();
        return .{ .FunctionDeclaration = decl };
    }

    fn parse_param_list(p: *ParserState, prev: *?*ast.ParamList) !void {
        var param = p.new_node(ast.ParamList);
        param.next = null;

        param.name_tk = try p.assert_token_is(.{ .Identifier = "" });
        _ = try p.assert_token_is(.Colon);

        param.typ = try TypeParser.parse(p);

        prev.* = param;

        if (TokenTag.eq(p.token.tag, .Rparen) or TokenTag.eq(p.token.tag, .Rbrace)) return;
        _ = try p.assert_token_is(.Comma);
        if (TokenTag.eq(p.token.tag, .Rparen) or TokenTag.eq(p.token.tag, .Rbrace)) return; //for trailing commmas

        return parse_param_list(p, &param.next);
    }
};

const StatementParser = struct {
    fn parse(p: *ParserState) !ast.Statement {
        const initial = p.token;
        switch (initial.tag) {
            .Return => {
                try p.adv();
                return .{ .ReturnStatement = try ExpressionParser.parse(p, .Newline) };
            },
            .Identifier => {
                switch (p.peek_token.tag) {
                    .Dot, .Equal => {
                        return try parse_assignment(p);
                    },
                    .Colon => {
                        try p.adv();
                        try p.adv();
                        return try parse_var_decl(p, initial);
                    },
                    else => return .{ .ExpressionStatement = try ExpressionParser.parse(p, .Newline) },
                }
            },
            .If => {
                try p.adv();
                return try parse_conditional(p, false, initial.location);
            },
            .While => {
                try p.adv();
                return try parse_conditional(p, true, initial.location);
            },
            .Hat => {
                return try parse_assignment(p);
            },
            .Newline => {
                try p.adv();
                return try parse(p);
            },
            else => {
                std.log.err("Cannot begin statement with {t}", .{initial});
                return ParseError.UnexpectedToken;
            },
        }
    }

    fn parse_assignment(p: *ParserState) !ast.Statement {
        const new_node = p.new_node(ast.VariableAssignmentNode);
        new_node.lhs = try ExpressionParser.parse(p, .Equal);
        new_node.loc = p.token.location;
        new_node.rhs = try ExpressionParser.parse(p, .Newline);
        return .{ .VariableAssignment = new_node };
    }

    fn parse_var_decl(p: *ParserState, name_tk: Token) !ast.Statement {
        var node = p.new_node(ast.VariableDeclarationNode);
        node.name_tk = name_tk;
        node.assignment = undefined;
        node.typ = null;

        if (TokenTag.eq(p.token.tag, .Equal)) { // x := <expr>
            try p.adv();
            node.assignment = try ExpressionParser.parse(p, .Newline);
        } else { // x: <type>
            node.typ = try TypeParser.parse(p);
            _ = try p.assert_token_is(.Equal);
            node.assignment = try ExpressionParser.parse(p, .Newline);
        }

        return .{ .VariableDeclaration = node };
    }

    fn parse_conditional(p: *ParserState, comptime is_while: bool, start_loc: FileLocation) ParseError!ast.Statement {
        const condition = try ExpressionParser.parse(p, .Lbrace);

        var body = std.ArrayList(ast.Statement).init(p.node_arena.allocator());

        while (!TokenTag.eq(p.token.tag, .Rbrace)) {
            const s = StatementParser.parse(p) catch {
                p.errored = true;
                p.skip_line() orelse break;
                continue;
            };
            try body.append(s);
        }
        _ = try p.assert_token_is(.Rbrace);

        if (is_while) {
            var node = p.new_node(ast.WhileStatementNode);
            node.start_loc = start_loc;
            node.condition = condition;
            node.body = try body.toOwnedSlice();
            return .{ .WhileStatement = node };
        }

        var node = p.new_node(ast.IfStatementNode);
        node.start_loc = start_loc;
        node.condition = condition;
        node.body = try body.toOwnedSlice();

        return .{ .IfStatement = node };
    }
};

// Most of expression parsing based off the building an interpreter in go book.
const ExpressionParser = struct {
    fn parse(p: *ParserState, end: TokenTag) !ast.Expression {
        const expr = try parse_precedence(p, .Lowest);
        try p.expect_delimiter(end);
        return expr;
    }

    fn is_infix_op(typ: TokenTag) bool {
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
            .DoubleDot,
            => true,
            else => false,
        };
    }

    fn parse_precedence(p: *ParserState, prec: Precedence) ParseError!ast.Expression {
        var expr: ast.Expression = switch (p.token.tag) {
            .Integer => .{ .LiteralInt = p.token },
            .Float => .{ .LiteralFloat = p.token },
            .True, .False => .{ .LiteralBool = p.token },
            .String => .{ .LiteralString = p.token },
            .Hat, .ExclamationMark, .Ampersand, .Dash => try parse_prefix(p),
            .Lparen => try parse_grouped(p),
            .Lbrace => try parse_array(p),
            .Cast => try parse_cast(p),
            .Identifier => |text| blk: {
                break :blk switch (p.peek_token.tag) {
                    .Lparen => try parse_call(p),
                    .Lbrace => blk2: {
                        //TODO: fix this
                        //only parse a record if you have x{, x { is not considered a record but is a block
                        if (p.token.location.col + text.len == p.peek_token.location.col and p.token.location.row == p.peek_token.location.row) {
                            break :blk2 try parse_record(p);
                        }
                        break :blk2 .{ .IdentifierInvokation = p.token };
                    },
                    else => .{ .IdentifierInvokation = p.token },
                };
            },
            else => {
                std.log.err("Cannot start expression with token {s}", .{p.token});
                return ParseError.UnexpectedToken;
            },
        };
        while (!TokenTag.eq(p.peek_token.tag, .Newline) and @intFromEnum(prec) < Precedence.from(p.peek_token.tag)) {
            if (!is_infix_op(p.peek_token.tag)) return expr;
            try p.adv();
            expr = try parse_infix(p, expr);
        }

        return expr;
    }

    fn parse_prefix(p: *ParserState) !ast.Expression {
        var expr = p.new_node(ast.UnaryExpressionNode);
        expr.op = p.token;

        try p.adv();

        expr.expr = try parse_precedence(p, .Prefix);
        return .{ .UnaryExpression = expr };
    }

    fn parse_infix(p: *ParserState, lhs: ast.Expression) !ast.Expression {
        var expr = p.new_node(ast.BinaryExpressionNode);
        expr.lhs = lhs;
        expr.op = p.token;

        const prec = Precedence.from(p.token.tag);
        try p.adv();

        //dot is right associative subtracting 1 from the precedence seems to work for that
        if (TokenTag.eq(expr.op.tag, .Dot)) {
            expr.rhs = try parse_precedence(p, @enumFromInt(prec - 1));
        } else {
            expr.rhs = try parse_precedence(p, @enumFromInt(prec));
        }

        if (TokenTag.eq(expr.op.tag, .Dot)) {
            return .{ .AccessExpression = expr };
        } else if (TokenTag.eq(expr.op.tag, .DoubleDot)) {
            return .{ .RangeExpression = expr };
        }

        return .{ .BinaryExpression = expr };
    }

    //group expressions are those in parenthesis
    fn parse_grouped(p: *ParserState) !ast.Expression {
        try p.adv(); //consume lparen
        var expr = try parse_precedence(p, .Lowest);
        _ = try p.expect(.Rparen);
        return expr;
    }

    fn parse_call(p: *ParserState) !ast.Expression {
        var expr = p.new_node(ast.FunctionInvokationNode);
        expr.name_tk = p.token;
        expr.args_list = null;

        var ll_head: ?*ast.ExprList = null;

        try p.adv(); //lparen cur token
        try p.adv(); //first argument / rparen is cur token

        if (TokenTag.eq(p.token.tag, .Rparen)) return .{ .FunctionInvokation = expr };

        try parse_arg(p, &ll_head, .Rparen);
        try p.adv(); //consume rparen

        var array = std.ArrayList(ast.Expression).init(p.node_arena.allocator());
        while (ll_head) |node| {
            array.append(node.expr) catch @panic("FATAL COMPILER ERROR: Out of memory");
            ll_head = node.next;
        }

        expr.*.args_list = array.toOwnedSlice() catch @panic("FATAL COMPILER ERROR: Out of memory");

        return .{ .FunctionInvokation = expr };
    }

    fn parse_arg(p: *ParserState, prev: *?*ast.ExprList, end: TokenTag) !void {
        const arg = try parse_precedence(p, .Lowest);
        const new_node = p.new_node(ast.ExprList);

        new_node.next = null;
        new_node.expr = arg;
        prev.* = new_node;

        if (TokenTag.eq(p.peek_token.tag, end)) return;

        try p.expect_delimiter(.Comma);
        return parse_arg(p, &new_node.next, end);
    }

    fn parse_record(p: *ParserState) !ast.Expression {
        const name = try p.assert_token_is(.{ .Identifier = "" });
        var v: ?*ast.FieldList = null;
        try p.adv(); //consume lbrace
        try parse_field(p, &v);
        try p.adv(); //consume rbrace

        var fields = std.ArrayList(ast.Field).init(p.node_arena.allocator());
        while (v) |node| {
            fields.append(.{ .field = node.field, .expr = node.expr }) catch @panic("FATAL COMPILER ERROR: Out of memory");
            v = node.next;
        }

        const new_node = p.new_node(ast.RecordInitializationNode);
        new_node.name_tk = name;
        new_node.fields = fields.toOwnedSlice() catch @panic("FATAL COMPILER ERROR: Out of memory");
        if (new_node.fields.len <= 0) {
            std.log.err("record initialization must initialize fields near {s}", .{p.token});
            return ParseError.UnexpectedToken;
        }
        return .{ .RecordInitialization = new_node };
    }

    fn parse_array(p: *ParserState) !ast.Expression {
        try p.adv();
        var v: ?*ast.ExprList = null;
        try parse_arg(p, &v, .Rbrace); //generate linked list of entries
        _ = try p.expect(.Rbrace);

        var items = std.ArrayList(ast.Expression).init(p.node_arena.allocator());
        while (v) |node| {
            try items.append(node.expr);
            v = node.next;
        }

        return .{ .ArrayInitialization = try items.toOwnedSlice() };
    }

    fn parse_cast(p: *ParserState) !ast.Expression {
        const cast_expr = p.new_node(ast.CastExpressionNode);
        try p.expect_delimiter(.Lparen);
        cast_expr.destination_type = try TypeParser.parse(p);
        _ = try p.assert_token_is(.Comma);
        cast_expr.expr = try ExpressionParser.parse_precedence(p, .Lowest);
        _ = try p.expect(.Rparen);
        return .{ .Cast = cast_expr };
    }

    fn parse_field(p: *ParserState, prev: *?*ast.FieldList) !void {
        const new_node = p.new_node(ast.FieldList);

        new_node.next = null;
        new_node.field = try p.assert_token_is(.{ .Identifier = "" });
        _ = try p.assert_token_is(.Colon);
        new_node.expr = try parse_precedence(p, .Lowest);
        prev.* = new_node;

        if (TokenTag.eq(p.peek_token.tag, .Rbrace)) return;
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

    fn from(typ: TokenTag) usize {
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
            .DoubleDot => .Equals,
            else => .Lowest,
        };
        return @intFromEnum(prec);
    }
};
