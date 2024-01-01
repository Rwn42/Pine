const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

//TODO: better naming scheme for union tag vs type

pub const Declaration = union(enum) {
    FuncDecl: *FunctionDeclaration,
};

pub const FunctionDeclaration = struct {
    name_tk: Token,
    return_type_tk: Token, //may be optional down the line
    params: ?*ParamList, //head to a linked list
    body: Expression,
};

//linked list node
pub const ParamList = struct {
    name_tk: Token,
    type_tk: Token,
    next: ?*ParamList,
};

pub const Expression = union(enum) {
    BinaryExprNode: *BinaryExpression,
    UnaryExprNode: *UnaryExpression,
    FuncCall: *FunctionCallExpression,
    LiteralInt: Token,
    LiteralFloat: Token,
    LiteralBool: Token,
    LiteralString: Token,
    IdentifierUsage: Token,

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .LiteralBool, .LiteralFloat, .LiteralString, .LiteralInt, .IdentifierUsage => |tk| try writer.print("{s}", .{tk}),
            .BinaryExprNode => |expr| {
                try writer.print("( ", .{});
                try expr.lhs.format(fmt, options, writer);
                try writer.print(" )", .{});
                try writer.print(" {s} ", .{expr.op.tagRepr.?});
                try writer.print("( ", .{});
                try expr.rhs.format(fmt, options, writer);
                try writer.print(" )", .{});
            },
            .UnaryExprNode => |expr| {
                try writer.print("{s}", .{expr.op.tagRepr.?});
                try writer.print("( ", .{});
                try expr.expr.format(fmt, options, writer);
                try writer.print(" )", .{});
            },
            .FuncCall => |expr| {
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

pub const BinaryExpression = struct {
    lhs: Expression,
    rhs: Expression,
    op: Token,
};

pub const UnaryExpression = struct {
    op: Token,
    expr: Expression,
};

pub const FunctionCallExpression = struct {
    name_tk: Token,
    args_list: ?*ExprList, //head node of a linked list
};

//linked list node
pub const ExprList = struct {
    expr: Expression,
    next: ?*ExprList,
};
