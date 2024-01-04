const std = @import("std");

const Location = @import("common.zig").Location;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

// Declarations

pub const Declaration = union(enum) {
    FunctionDeclaration: *FunctionDeclarationNode,
    RecordDeclaration: *RecordDeclarationNode,
    ConstantDeclaration: *ConstantDeclarationNode,

    pub fn format(self: Declaration, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt; // autofix
        _ = options; // autofix
        switch (self) {
            .FunctionDeclaration => |decl| {
                try writer.print("fn {s}\n", .{decl.name_tk.tag});

                if (decl.params) |p| try writer.print("    params: {s}\n", .{p});

                if (decl.return_type_tk) |tk| {
                    try writer.print("    return: {s}\n", .{tk.tag});
                } else try writer.print("    return: unspecified\n", .{});

                try writer.print("    body:\n", .{});
                for (decl.body) |s| {
                    try writer.print("        {s}\n", .{s});
                }
            },
            .RecordDeclaration => |decl| {
                try writer.print("    fields: {s}\n", .{decl.fields});
            },
            .ConstantDeclaration => |decl| {
                try writer.print("constant: {s} :: {s}", .{ decl.name_tk.tag, decl.value });
            },
        }
    }
};

//name :: fn(p1: type, p2: type) optional type {body}
pub const FunctionDeclarationNode = struct {
    name_tk: Token,
    return_type_tk: ?Token,
    params: ?*ParamList,
    body: []Statement,
};

//name :: record{field1: type, field2: type}
pub const RecordDeclarationNode = struct {
    name_tk: Token,
    fields: *ParamList,
};

pub const ParamList = struct {
    name_tk: Token,
    type_tk: Token,
    next: ?*ParamList,

    //has a formatter because printing these recursively is cleaner then a while loop in the union
    pub fn format(self: ParamList, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} : {s}, ", .{ self.name_tk.tag, self.type_tk.tag });
        if (self.next) |next| try next.format(fmt, options, writer);
    }
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
    VariableDeclaration: *VariableDeclarationNode,
    VariableAssignment: *VariableAssignmentNode,

    pub fn format(self: Statement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .ExpressionStatement => |expr| try expr.format(fmt, options, writer),
            .ReturnStatement => |expr| {
                try writer.print("return: {s}", .{expr});
            },
            .VariableDeclaration => |decl| {
                try writer.print("variable declaration: {s} ", .{decl.name_tk.tag});
                if (decl.type_tk) |typ| try writer.print("type: {s} ", .{typ.tag});
                if (decl.assignment) |expr| {
                    try writer.print("body: {s}", .{expr});
                }
            },
            .VariableAssignment => |decl| {
                try writer.print("variable assignment: {s}", .{decl.name_tk.tag});
                try writer.print(" body: {s}", .{decl.assignment});
            },
        }
    }
};

pub const VariableDeclarationNode = struct {
    name_tk: Token,
    type_tk: ?Token,
    assignment: ?Expression,
};

pub const VariableAssignmentNode = struct {
    name_tk: Token,
    assignment: Expression,
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
            .LiteralBool, .LiteralFloat, .LiteralString, .LiteralInt, .IdentifierInvokation => |tk| try writer.print("{s}", .{tk.tag}),
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
                try writer.print("{s} ( ", .{expr.name_tk.tag});
                defer writer.print(")", .{}) catch {};
                if (expr.args_list) |list| try writer.print("{s}", .{list});
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

    pub fn format(self: ExprList, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}, ", .{self.expr});
        if (self.next) |next| try next.format(fmt, options, writer);
    }
};
