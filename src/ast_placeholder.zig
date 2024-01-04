const std = @import("std");

const Location = @import("common.zig").Location;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

// Declarations

pub const Declaration = union(enum) {
    FunctionDelcaration: *FunctionDeclarationNode,
    RecordDeclaration: *RecordDeclarationNode,
    ConstantDeclaration: *ConstantDeclarationNode,
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
};

pub const BinaryExpressionNode = struct {
    lhs: Expression,
    rhs: Expression,
    op: Token,
};

//!a
pub const UnaryExpressionNode = struct {
    lhs: Expression,
    rhs: Expression,
    op: Token,
};

pub const FunctionInvokationNode = struct {
    name_tk: Token,
    args_list: ?*ExprList,
};

pub const ExprList = struct {
    expr: Expression,
    next: ?*ExprList,
};
