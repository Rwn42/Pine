const std = @import("std");

const Token = @import("token.zig").Token;
const FileLocation = @import("token.zig").FileLocation;

pub const AST = struct {
    functions: []*FunctionDeclarationNode,
    records: []*RecordDeclarationNode,
    foreign: []*ForeignDeclarationNode,
    imports: []Token,
    constants: []*ConstantDeclarationNode,
};

//NOTE: ---- TOP LEVEL CODE ----

pub const Declaration = union(enum) {
    FunctionDeclaration: *FunctionDeclarationNode,
    RecordDeclaration: *RecordDeclarationNode,
    ConstantDeclaration: *ConstantDeclarationNode,
    ForeignDeclaration: *ForeignDeclarationNode,
    ImportDeclaration: Token,

    pub fn format(self: Declaration, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .FunctionDeclaration => |decl| {
                try writer.print("fn {s}\n", .{decl.name_tk.tag});
                try writer.print("public: {any} ", .{decl.public});
                try writer.print("params: ", .{});
                for (decl.params) |p| try writer.print("{s} ", .{p});

                if (decl.return_typ) |t| {
                    try writer.print("    return: {s}\n", .{t});
                } else {
                    try writer.print("    infered", .{});
                }

                try writer.print("    body:\n", .{});
                for (decl.body) |s| {
                    try writer.print("        {s}\n", .{s});
                }
            },
            .RecordDeclaration => |decl| {
                try writer.print("record {s}\n", .{decl.name_tk.tag});
                try writer.print("public: {any} ", .{decl.public});

                try writer.print("params: ", .{});
                for (decl.fields) |p| try writer.print("{s} ", .{p});
            },
            .ConstantDeclaration => |decl| {
                try writer.print("constant: {s} :: {s}", .{ decl.name_tk.tag, decl.value });
            },
            .ImportDeclaration => |decl| {
                try writer.print("importing {s}", .{decl});
            },
            .ForeignDeclaration => |decl| {
                try writer.print("foreign library {s} importing ...", .{decl.library});
                for (decl.function_imports) |import| {
                    try writer.print("{s}, ", .{import});
                }
                try writer.print("\n", .{});
            },
        }
    }
};

//name :: fn(p1: type, p2: type) optional type {body}
pub const FunctionDeclarationNode = struct {
    name_tk: Token,
    return_typ: ?DefinedType,
    public: bool,
    params: []Param,
    body: []Statement,
};

//name :: record{field1: type, field2: type}
pub const RecordDeclarationNode = struct {
    name_tk: Token,
    public: bool,
    fields: []Param,
};

//#foreign "libc" ["printf", "malloc"]
pub const ForeignDeclarationNode = struct {
    library: Token,
    function_imports: []Token,
};

//name :: value or maybe name :: type one day
pub const ConstantDeclarationNode = struct {
    name_tk: Token,
    value: Expression,
};

pub const ParamList = struct {
    name_tk: Token,
    typ: DefinedType,
    next: ?*ParamList,

    //has a formatter because printing these recursively is cleaner then a while loop in the union
    pub fn format(self: ParamList, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} : {s}, ", .{ self.name_tk.tag, self.typ });
        if (self.next) |next| try next.format(fmt, options, writer);
    }
};

// these are typed parameters for function params and record fields name: type
pub const Param = struct {
    name_tk: Token,
    typ: DefinedType,

    pub fn format(self: Param, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} : {s}, ", .{ self.name_tk.tag, self.typ });
    }
};

//NOTE: ---- TYPES ----

pub const DefinedType = union(enum) {
    Array: *ArrayType,
    Pointer: *PointerType,
    Basic: Token,
    WidePointer: *PointerType,

    pub fn format(self: DefinedType, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Array => |arr| {
                try writer.print("array of length {d} of {s}", .{ arr.length, arr.element_typ });
            },
            .Pointer => |pointing| {
                try writer.print("^{s}", .{pointing.pointing_to});
            },
            .WidePointer => |pointing| {
                try writer.print("[]{s}", .{pointing.pointing_to});
            },
            .Basic => |tk| try writer.print("{s}", .{tk}),
        }
    }
};

pub const PointerType = struct {
    pointing_to: DefinedType,
};

pub const ArrayType = struct {
    length: Expression,
    element_typ: DefinedType,
};

//NOTE: ---- STATEMENTS ----

pub const Statement = union(enum) {
    ExpressionStatement: Expression,
    ReturnStatement: ?Expression,
    VariableDeclaration: *VariableDeclarationNode,
    VariableAssignment: *VariableAssignmentNode,
    IfStatement: *IfStatementNode,
    WhileStatement: *WhileStatementNode,

    pub fn format(self: Statement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .ExpressionStatement => |expr| try writer.print("expr statement: {s}", .{expr}),
            .ReturnStatement => |expr| {
                if (expr) |real_expr| try writer.print("return: {s}", .{real_expr});
            },
            .VariableDeclaration => |decl| {
                try writer.print("variable declaration: {s} ", .{decl.name_tk.tag});
                if (decl.typ) |typ| try writer.print("type: {s} ", .{typ});
                try writer.print("body: {s}", .{decl.assignment});
            },
            .VariableAssignment => |decl| {
                try writer.print("variable assignment: {s} {s}", .{ decl.lhs, decl.loc });
                try writer.print(" body: {s}", .{decl.rhs});
            },
            .IfStatement => |stmt| {
                try writer.print("if: {s} do: ", .{stmt.condition});
                for (stmt.body) |stmt2| {
                    try stmt2.format(fmt, options, writer);
                }
            },
            .WhileStatement => |stmt| {
                try writer.print("while: {s} do: ", .{stmt.condition});
                for (stmt.body) |stmt2| {
                    try stmt2.format(fmt, options, writer);
                }
            },
        }
    }
};

pub const VariableDeclarationNode = struct {
    name_tk: Token,
    typ: ?DefinedType,
    assignment: Expression,
};

pub const VariableAssignmentNode = struct {
    lhs: Expression,
    rhs: Expression,
    loc: FileLocation,
};

pub const IfStatementNode = struct {
    start_loc: FileLocation,
    condition: Expression,
    body: []Statement,
};

pub const WhileStatementNode = struct {
    start_loc: FileLocation,
    condition: Expression,
    body: []Statement,
};

//NOTE: ---- EXPRESSIONS ----

pub const Expression = union(enum) {
    BinaryExpression: *BinaryExpressionNode,
    AccessExpression: *BinaryExpressionNode,
    RangeExpression: *BinaryExpressionNode,
    UnaryExpression: *UnaryExpressionNode,
    FunctionInvokation: *FunctionInvokationNode,
    ArrayInitialization: []Expression,
    RecordInitialization: *RecordInitializationNode,
    Cast: *CastExpressionNode,
    LiteralInt: Token,
    LiteralFloat: Token,
    LiteralBool: Token,
    LiteralString: Token,
    IdentifierInvokation: Token,

    //location is only used for error reporting so doing a switch every time is fine
    pub fn location(self: Expression) FileLocation {
        switch (self) {
            .LiteralBool, .LiteralFloat, .LiteralString, .LiteralInt, .IdentifierInvokation => |tk| return tk.location,
            .BinaryExpression, .AccessExpression, .RangeExpression => |bin_expr| return bin_expr.op.location,
            .UnaryExpression => |un_expr| return un_expr.op.location,
            .ArrayInitialization => |a_expr| return Expression.location(a_expr[0]),
            .RecordInitialization => |r_expr| return r_expr.name_tk.location,
            .FunctionInvokation => |c_expr| return c_expr.name_tk.location,
            .Cast => |c_expr| return Expression.location(c_expr.expr),
        }
    }

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .LiteralBool, .LiteralFloat, .LiteralString, .LiteralInt, .IdentifierInvokation => |tk| try writer.print("{s}", .{tk.tag}),
            .BinaryExpression, .AccessExpression, .RangeExpression => |expr| {
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
                try writer.print(" call {s} ( ", .{expr.name_tk.tag});
                defer writer.print(")", .{}) catch {};
                if (expr.args_list) |args_list| {
                    for (args_list) |arg| try writer.print("{s} ", .{arg});
                }
            },
            .ArrayInitialization => |expr| {
                for (expr) |arg| try writer.print("{s} ", .{arg});
            },
            .RecordInitialization => |expr| {
                try writer.print("record init: type {s},  {s}", .{ expr.name_tk, expr.fields });
            },
            .Cast => |cast| {
                try writer.print("casting {s} to {s}", .{ cast.expr, cast.destination_type });
            },
        }
    }
};

pub const CastExpressionNode = struct {
    destination_type: DefinedType,
    expr: Expression,
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
    args_list: ?[]Expression,
};

pub const RecordInitializationNode = struct {
    name_tk: Token,
    fields: []Field,
};

pub const Field = struct {
    field: Token,
    expr: Expression,
    pub fn format(self: Field, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} : {s}, ", .{ self.field.tag, self.expr });
    }
};

pub const FieldList = struct {
    field: Token,
    expr: Expression,
    next: ?*FieldList,

    pub fn format(self: FieldList, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} : {s}, ", .{ self.field.tag, self.expr });
        if (self.next) |next| try next.format(fmt, options, writer);
    }
};

pub const ExprList = struct {
    expr: Expression,
    next: ?*ExprList,
};
