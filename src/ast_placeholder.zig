const std = @import("std");

const Location = @import("common.zig").Location;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

// Declarations

pub const Declaration = union(enum) {
    FunctionDeclaration: *FunctionDeclarationNode,
    RecordDeclaration: *RecordDeclarationNode,
    ConstantDeclaration: *ConstantDeclarationNode,
};

//name :: fn(p1: type, p2: type) optional type {body}
pub const FunctionDeclarationNode = struct {
    name_tk: Token,
    return_type_tk: ?Token,
    params: ?*ParamList,
    body: Statement,
};

//name :: record{field1: type, field2: type}
pub const RecordDeclarationNode = struct {
    name_tk: Token,
    fields: ?*ParamList,
};

pub const ParamList = struct {
    name_tk: Token,
    type_tk: Token,
    next: ?*ParamList,
};

//name :: value
pub const ConstantDeclarationNode = struct {
    name_tk: Token,
    value: Expression,
};

// Statements

pub const Statement = union(enum) {
    ExpressionStatement: Expression,
    ReturnStatement: Expression,
};

// Expressions

pub const Expression = union(enum) {
    BinaryExpression: *BinaryExpressionNode,
    UnaryExpression: *UnaryExpressionNode,
    FunctionInvokation: *FunctionInvokationNode,
    LiteralInt: Token,
    LiteralFloat: Token,
    LiteralBool: Token,
    LiteralString: Token,
    IdentifierInvokation: Token,

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .LiteralBool, .LiteralFloat, .LiteralString, .LiteralInt, .IdentifierInvokation => |tk| try writer.print("{s}", .{tk}),
            .BinaryExpression => |expr| {
                try writer.print("( ", .{});
                try expr.lhs.format(fmt, options, writer);
                try writer.print(" )", .{});
                try writer.print(" {s} ", .{expr.op.tag});
                try writer.print("( ", .{});
                try expr.rhs.format(fmt, options, writer);
                try writer.print(" )", .{});
            },
            .UnaryExpression => |expr| {
                try writer.print("{s}", .{expr.op.tag});
                try writer.print("( ", .{});
                try expr.expr.format(fmt, options, writer);
                try writer.print(" )", .{});
            },
            .FunctionInvokation => |expr| {
                try writer.print("{s} ( ", .{expr.name_tk});
                defer writer.print(")", .{}) catch {};
                if (expr.args_list == null) return;
                var node = expr.args_list.?;
                while (true) {
                    try writer.print("{s}, ", .{node.expr});
                    if (node.next) |next| {
                        node = next;
                    } else {
                        break;
                    }
                }
            },
        }
    }
};

// a + b;
pub const BinaryExpressionNode = struct {
    lhs: Expression,
    rhs: Expression,
    op: Token,
};

// !a;
pub const UnaryExpressionNode = struct {
    expr: Expression,
    op: Token,
};

// func(a + b);
pub const FunctionInvokationNode = struct {
    name_tk: Token,
    args_list: ?*ExprList,
};

pub const ExprList = struct {
    expr: Expression,
    next: ?*ExprList,
};
