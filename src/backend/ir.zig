const std = @import("std");

const typing = @import("typing.zig");
const lexing = @import("../frontend/lexer.zig");
const parsing = @import("../frontend/parser.zig");
const ast = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;

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

pub const IRInstruction = union(enum) {
    Function: FuncData,
    Push: usize,
    StoreW,
    StoreB,
    LoadW,
    LoadB,
    Ret,
    StackAddr: usize,
    StaticStart: usize,
    ReturnStart,
    Add_I,
    Add_F,
    Mul_I,
    Mul_F,
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
    static: []u8,
    instructions: []IRInstruction,

    pub fn deinit(ir: FileIR, allocator: std.mem.Allocator) void {
        allocator.free(ir.public);
        allocator.free(ir.external);
        allocator.free(ir.static);
        allocator.free(ir.instructions);
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
            .param = if (param) true else false,
        });
        self.stack_addr += typ.size;

        return self.scopes.top().get(name_tk.tag.Identifier).?;
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

        for (func_type.params, func.params) |p_info, ast_p| {
            _ = try self.register_var_decl(ast_p.name_tk, p_info, true);
        }
        self.stack_addr = 0; //params are done so reset stack_addr since param stack addressed are "negative"

        for (func.body) |stmt| {
            try self.generate_statement(stmt);
        }

        self.program.items[start_idx] = .{ .Function = .{ .name = func.name_tk.tag.Identifier, .stack_size = self.stack_addr } };
        self.stack_addr = 0;
    }

    fn generate_statement(self: *Self, input_stmt: ast.Statement) !void {
        switch (input_stmt) {
            .ReturnStatement => |expr| {
                try self.program.append(.ReturnStart);
                try self.program.append(.QuickStore);
                try self.generate_primary_expression(expr, 0);
                try self.program.append(.Ret);
            },
            .VariableAssignment => |assignment| {
                _ = try self.generate_lvalue(assignment.lhs);
                try self.program.append(.QuickStore);
                try self.generate_primary_expression(assignment.rhs, 0);
            },
            .VariableDeclaration => |stmt| {},
            else => {},
        }
    }

    fn generate_primary_expression(self: *Self, input_expr: ast.Expression, offset: usize) !typing.TypeInfo {
        switch (input_expr) {
            .LiteralInt => |int_tk| {
                try self.program.append(.{ .Const = @bitCast(int_tk.tag.Integer) });
                try self.program.append(.QuickLoad);
                try self.program.append(.{ .Const = offset });
                try self.program.append(.Add_I);
                return typing.PinePrimitive.get("untyped_int").?;
            },
            .IdentifierInvokation => {
                const info = try generate_lvalue(self, input_expr);
                try self.program.append(if (info.size == 8) .LoadW else .LoadB);
                if (!info.type_info.tag.is_trivial()) @panic("not implemented");
                try self.program.append(.QuickLoad);
                try self.program.append(.{ .Const = offset });
                try self.program.append(.Add_I);
                return info;
            },
        }
    }

    fn generate_lvalue(self: *Self, input_expr: ast.Expression) !typing.TypeInfo {
        switch (input_expr) {
            .IdentifierInvokation => |tk| {
                const info = try self.find_identifier(tk, self.scopes.top_idx());
                try self.program.append(.{ .StackAddr = info.stack_offset });
                return info.type_info;
            },
        }
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
