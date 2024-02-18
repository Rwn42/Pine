const std = @import("std");

const typing = @import("typing.zig");
const lexing = @import("../frontend/lexer.zig");
const parsing = @import("../frontend/parser.zig");
const ast = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;
const FileLocation = @import("../frontend/token.zig").FileLocation;

pub const IRError = error{
    Undeclared,
    OutOfMemory,
    Duplicate,
    Syntax,
};

const FuncData = struct {
    name: []const u8,
    stack_size: usize,
};

const CCall = struct {
    name: []const u8,
    param_n: usize,
};

pub const IRInstruction = union(enum) {
    Function: FuncData,
    PushB: u8,
    PushW: usize,
    Call: []const u8,
    CCall: CCall,
    StoreW,
    StoreB,
    LoadW,
    LoadB,
    Ret,
    TempStore,
    TempLoad,
    StackAddr: isize,
    StaticStart: usize,
    ReturnAddr: usize,
    Add_I: bool,
    Add_F: bool,
    Mul_I: bool,
    Mul_F: bool,
    LT_I,
    LT_F,
    GT_I,
    GT_F,
    EQ,
    Not,
};

pub const FileIR = struct {
    public: [][]const u8,
    external: [][]const u8,
    imported: [][]const u8,
    static: []u8,
    instructions: []IRInstruction,

    pub fn deinit(ir: FileIR, allocator: std.mem.Allocator) void {
        allocator.free(ir.public);
        allocator.free(ir.external);
        allocator.free(ir.static);
        allocator.free(ir.instructions);
        allocator.free(ir.imported);
    }
};

const VarInfo = struct {
    stack_offset: usize,
    type_info: typing.TypeInfo,
    parameter: bool,
};

const Block = std.StringHashMap(VarInfo);
const Scope = Stack(*Block, 32, "Too many Nested Blocks");

const ExecutionState = struct {
    const Self = @This();

    program: std.ArrayList(IRInstruction),
    static: std.ArrayList(u8),
    allocator: std.mem.Allocator,
    scopes: Scope = Scope.init(),
    types: *const typing.FileTypes,

    current_function_name: []const u8 = undefined,

    stack_addr: usize = 0,

    fn init(allocator: std.mem.Allocator, types: *const typing.FileTypes) Self {
        return .{
            .program = std.ArrayList(IRInstruction).init(allocator),
            .static = std.ArrayList(u8).init(allocator),
            .allocator = allocator,
            .types = types,
        };
    }

    fn deinit(self: *Self) void {
        self.static.deinit();
        self.program.deinit();
    }

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.scopes.get(idx).get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("Undeclared identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    fn program_len(self: *Self) usize {
        return self.program.items.len;
    }

    fn register_var_decl(self: *Self, name_tk: Token, typ: typing.TypeInfo, comptime param: bool) !VarInfo {
        if (self.scopes.top().contains(name_tk.tag.Identifier)) {
            std.log.err("Duplicate definition of identifier {s}", .{name_tk});
            return IRError.Duplicate;
        }
        try self.scopes.top().put(name_tk.tag.Identifier, .{
            .type_info = typ,
            .stack_offset = self.stack_addr,
            .parameter = if (param) true else false,
        });
        self.stack_addr += typ.size;

        return self.scopes.top().get(name_tk.tag.Identifier).?;
    }

    fn generate_mem_op(self: *Self, type_info: typing.TypeInfo, comptime load: bool, loc: FileLocation) !void {
        switch (type_info.tag) {
            .PineVoid => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .PineBool, .PineByte, .PineInt, .PineFloat, .PinePtr, .PineUntypedInt, .PineWord => {
                if (load) {
                    try self.program.append(if (type_info.size == 2) .LoadB else .LoadW);
                } else {
                    try self.program.append(if (type_info.size == 2) .StoreB else .StoreW);
                }
            },
            .PineRecord => |fields| {
                try self.program.append(.TempStore); //we will need the start addr of the struct many times
                const record_vals = fields.values();

                if (load) std.mem.reverse(typing.FieldInfo, record_vals);
                for (record_vals) |field| {
                    try self.program.append(.TempLoad);
                    try self.program.append(.{ .PushW = field.offset });
                    try self.program.append(.{ .Add_I = false });
                    try self.generate_mem_op(field.type_info, load, loc);
                }
            },
            .PineWidePointer => {
                try self.program.append(.{ .PushW = 8 });
                try self.program.append(.{ .Add_I = false });
                try self.generate_mem_op(typing.PinePrimitive.get("word").?, load, loc);
                try self.program.append(.TempStore);
                try self.generate_mem_op(.{ .child = null, .tag = .PinePtr, .size = 8 }, load, loc);
                try self.program.append(.TempLoad);
            },
            .PineArray => {
                const child_info = try self.types.from_ast(type_info.child.?);
                const length = type_info.size / child_info.size;
                try self.program.append(.TempStore); //we will need the start addr of array many times
                for (0..length) |i| {
                    try self.program.append(.TempLoad);
                    try self.program.append(.{ .PushW = i * child_info.size });
                    try self.program.append(.{ .Add_I = false });
                    try self.generate_mem_op(child_info, load, loc);
                }
            },
        }
    }

    fn generate_function(self: *Self, func: *ast.FunctionDeclarationNode) !void {
        self.current_function_name = func.name_tk.tag.Identifier;

        var scope = Block.init(self.allocator);
        self.scopes.push(&scope);
        defer scope.deinit();
        defer self.scopes.pop();

        const func_type = self.types.function_types.get(func.name_tk.tag.Identifier).?;
        const start_idx = self.program_len();
        try self.program.append(.{ .Function = undefined });

        self.stack_addr += func_type.return_type.size;
        for (func_type.params, func.params) |p_info, ast_p| {
            _ = try self.register_var_decl(ast_p.name_tk, p_info, true);
        }
        self.stack_addr = 0; //params are done so reset stack_addr since param stack addressed are "negative"

        for (func.body) |stmt| {
            try StatementGenerator.generate(self, stmt);
        }
        try self.program.append(.Ret); //does not help if function returns a value but the user did not

        self.program.items[start_idx] = .{ .Function = .{ .name = func.name_tk.tag.Identifier, .stack_size = self.stack_addr } };
        self.stack_addr = 0;
    }
};

const StatementGenerator = struct {
    fn generate(s: *ExecutionState, input_stmt: ast.Statement) IRError!void {
        switch (input_stmt) {
            .ReturnStatement => |expr| {
                var expr_info = typing.PinePrimitive.get("void").?;
                if (expr) |real_expr| {
                    const function_type = s.types.function_types.get(s.current_function_name).?.return_type;
                    expr_info = try ExpressionGenerator.generate_rvalue(s, real_expr);
                    try s.program.append(.{ .ReturnAddr = function_type.size });
                    try typing.equivalent(expr_info, function_type);
                    try s.generate_mem_op(expr_info, false, real_expr.location());
                }
                try s.program.append(.Ret);
            },
            .ExpressionStatement => |expr| {
                _ = try ExpressionGenerator.generate_rvalue(s, expr);
            },
            .VariableDeclaration => |stmt| {
                var given_info = try ExpressionGenerator.generate_rvalue(s, stmt.assignment);

                if (stmt.typ) |typ| {
                    const declared_info = try s.types.from_ast(typ);
                    try typing.equivalent(given_info, declared_info);
                    given_info = declared_info;
                }
                const info = try s.register_var_decl(stmt.name_tk, given_info, false);
                try s.program.append(.{ .StackAddr = @intCast(info.stack_offset) });
                try s.generate_mem_op(given_info, false, stmt.name_tk.location);
            },
            .VariableAssignment => |stmt| {
                const rhs_info = try ExpressionGenerator.generate_rvalue(s, stmt.rhs);
                const lhs_info = try ExpressionGenerator.generate_lvalue(s, stmt.lhs);
                try s.generate_mem_op(lhs_info, false, stmt.loc);
                try typing.equivalent(rhs_info, lhs_info);
            },
            else => {},
        }
    }
};

const ExpressionGenerator = struct {
    fn generate_rvalue(s: *ExecutionState, input_expr: ast.Expression) IRError!typing.TypeInfo {
        switch (input_expr) {
            .LiteralInt => |tk| {
                try s.program.append(.{ .PushW = @bitCast(tk.tag.Integer) });
                return typing.PinePrimitive.get("untyped_int").?;
            },
            .LiteralString => |tk| {
                try s.program.append(.{ .StaticStart = s.static.items.len });
                try s.static.appendSlice(tk.tag.String);
                try s.static.append(10);
                try s.static.append(0);
                return typing.PinePrimitive.get("cstring").?;
            },
            .IdentifierInvokation => |tk| {
                const info = try generate_lvalue(s, input_expr);
                try s.generate_mem_op(info, true, tk.location);
                return info;
            },
            .BinaryExpression => |expr| {
                const lhs_info = try generate_rvalue(s, expr.lhs);
                const rhs_info = try generate_rvalue(s, expr.rhs);
                try typing.equivalent(rhs_info, lhs_info);
                switch (expr.op.tag) {
                    .Plus => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Add_F = false } else .{ .Add_I = false }),
                    .Asterisk => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Mul_F = false } else .{ .Mul_I = false }),
                    .Dash => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Add_F = true } else .{ .Add_I = true }),
                    .SlashForward => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Mul_F = true } else .{ .Mul_I = true }),
                    else => {},
                }
                return lhs_info;
            },
            .FunctionInvokation => |expr| {
                if (expr.args_list) |args| {
                    std.mem.reverse(ast.Expression, args);
                    for (args) |arg| {
                        _ = try ExpressionGenerator.generate_rvalue(s, arg);
                    }
                }

                if (s.types.find_function(expr.name_tk)) |info| {
                    try s.program.append(.{ .Call = expr.name_tk.tag.Identifier });
                    return info.return_type;
                } else {
                    if (expr.args_list) |args| {
                        try s.program.append(.{ .CCall = .{ .name = expr.name_tk.tag.Identifier, .param_n = args.len } });
                    } else {
                        try s.program.append(.{ .CCall = .{ .name = expr.name_tk.tag.Identifier, .param_n = 0 } });
                    }
                    return typing.PinePrimitive.get("word").?;
                }
            },
            else => {},
        }
        return IRError.OutOfMemory;
    }

    fn generate_lvalue(s: *ExecutionState, input_expr: ast.Expression) IRError!typing.TypeInfo {
        switch (input_expr) {
            .IdentifierInvokation => |tk| {
                const info = try s.find_identifier(tk, s.scopes.top_idx());
                const onen: isize = -1;
                const one: isize = 1;
                try s.program.append(.{ .StackAddr = @as(isize, @intCast(info.stack_offset)) * if (info.parameter) onen else one });
                return info.type_info;
            },
            .UnaryExpression => |expr| {
                if (expr.op.tag != .Hat) {
                    std.log.err("Cannot use {s} as an lvalue", .{input_expr});
                    return IRError.Syntax;
                }
                const info = try generate_lvalue(s, expr.expr);
                if (info.tag != .PinePtr) {
                    std.log.err("attempt to dereference non pointer {s}", .{input_expr});
                    return IRError.Syntax;
                }
                try s.program.append(.LoadW);
            },
            else => {},
        }
        return IRError.OutOfMemory;
    }
};

pub fn generate_file_ir(types: typing.FileTypes, file_ast: ast.AST, allocator: std.mem.Allocator) IRError!FileIR {
    var ir: FileIR = undefined;

    var extern_buffer = std.ArrayList([]const u8).init(allocator);
    defer extern_buffer.deinit();
    for (file_ast.foreign) |foreign_decl| {
        for (foreign_decl.function_imports) |lib_token| {
            try extern_buffer.append(lib_token.tag.String);
        }
    }
    ir.external = try extern_buffer.toOwnedSlice();

    var imported_buffer = std.ArrayList([]const u8).init(allocator);
    defer imported_buffer.deinit();
    for (types.imported_types) |imported| {
        for (imported.function_types.values(), imported.function_types.keys()) |func_type, name| {
            if (func_type.public) {
                try imported_buffer.append(name);
            }
        }
    }

    ir.imported = try imported_buffer.toOwnedSlice();

    var public_buffer = std.ArrayList([]const u8).init(allocator);
    defer public_buffer.deinit();
    var func_type_iter = types.function_types.iterator();
    while (func_type_iter.next()) |f_type| {
        if (f_type.value_ptr.public) {
            try public_buffer.append(f_type.key_ptr.*);
        }
    }
    ir.public = try public_buffer.toOwnedSlice();

    var execution_state = ExecutionState.init(allocator, &types);
    defer execution_state.deinit();

    for (file_ast.functions) |f_decl| {
        try execution_state.generate_function(f_decl);
    }

    ir.instructions = execution_state.program.toOwnedSlice() catch @panic("Out of memory!");
    ir.static = execution_state.static.toOwnedSlice() catch @panic("Out of memory!");

    return ir;
}

//general purpose stack
pub fn Stack(comptime T: type, comptime limit: usize, comptime msg: []const u8) type {
    return struct {
        const Self = @This();
        const overflow_message = msg;

        sp: usize,
        buffer: [limit]T,

        pub fn init() Self {
            return .{
                .sp = 0,
                .buffer = undefined,
            };
        }

        //only push could happen user side an under flow or out of bounds access would be my fault
        pub fn push(self: *Self, value: T) void {
            if (self.sp >= limit - 1) {
                std.log.err("{s}", .{overflow_message});
                @panic("FATAL COMPILER ERROR: Stack Overflow");
            }
            self.buffer[self.sp] = value;
            self.sp += 1;
        }

        pub fn pop(self: *Self) void {
            if (self.sp <= 0) {
                @panic("FATAL COMPILER ERROR: Stack Underflow");
            }
            self.sp -= 1;
        }

        pub fn pop_ret(self: *Self) T {
            const a = self.top();
            if (self.sp <= 0) {
                @panic("FATAL COMPILER ERROR: Stack Underflow");
            }
            self.sp -= 1;
            return a;
        }

        pub fn top(self: *Self) T {
            return self.buffer[self.sp - 1];
        }

        pub fn top_idx(self: *Self) usize {
            return self.sp - 1;
        }

        pub fn get(self: *Self, idx: usize) T {
            if (idx < 0 or idx >= limit) @panic("Out of bounds access");
            return self.buffer[idx];
        }
    };
}
