const std = @import("std");

const ast = @import("../frontend/ast.zig");
const typing = @import("./typing.zig");

pub const IRError = error{
    Undeclared,
    OutOfMemory,
    Duplicate,
};

pub const IR = struct {
    instructions: []IRInstruction,
    static: []u8,
    externals: [][]const u8,
    function_names: [][]const u8,

    pub fn deinit(ir: *IR, allocator: std.mem.Allocator) void {
        allocator.free(ir.instructions);
        allocator.free(ir.static);
        allocator.free(ir.externals);
        allocator.free(ir.function_names);
    }
};

pub const IRInstruction = union(enum) {
    Function,
    Load,
    Store,
    StaticAddress,
    StackAddress,
    Push,
    Add_I,
    Add_F,
    Mul_I,
    Mul_F,
    Lt,
    Le,
    Eq,
    Call,
    Ret,
};

pub const IRGenerator = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    input_ast: ast.AST,
    types: typing.Types,

    linked_libraries: std.ArrayList([]const u8),
    externals: std.ArrayList([]const u8),

    program: std.ArrayList(IRInstruction),
    static: std.ArrayList(u8),

    pub fn init(input_ast: ast.AST, types: typing.Types, allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .input_ast = input_ast,
            .types = types,
            .linked_libraries = std.ArrayList([]const u8).init(allocator),
            .externals = std.ArrayList([]const u8).init(allocator),
            .program = std.ArrayList(IRInstruction).init(allocator),
            .static = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn generate(self: *Self) IRError!IR {
        try self.generate_ir_externals();
        try self.generate_ir_executable();
        return .{
            .instructions = try self.program.toOwnedSlice(),
            .static = try self.static.toOwnedSlice(),
            .externals = try self.externals.toOwnedSlice(),
            .function_names = self.types.function_types.keys(),
        };
    }

    pub fn deinit(self: *Self) void {
        self.linked_libraries.deinit();
        self.externals.deinit();
        self.static.deinit();
        self.program.deinit();
    }

    fn generate_ir_externals(self: *Self) !void {
        for (self.input_ast.foreign) |foreign_decl| {
            try self.linked_libraries.append(foreign_decl.library.tag.String);
            for (foreign_decl.function_imports) |external_function| {
                try self.externals.append(external_function.tag.String);
            }
        }
    }

    fn generate_ir_executable(self: *Self) !void {
        var exec_generator = ExecutableIRGenerator.init(self);
        for (self.input_ast.functions) |f_decl| {
            try exec_generator.generate_function(f_decl);
        }
    }
};

const VarInfo = struct {
    type_info: typing.TypeInfo,
    stack_addr: usize,
};

const Block = std.StringHashMap(VarInfo);

const ExecutableIRGenerator = struct {
    const Self = @This();

    parent: *IRGenerator,

    scope: Stack(Block, 32, "Too many nested blocks:") = Stack(Block, 32, "Too many nested blocks:").init(),

    fn init(parent: *IRGenerator) Self {
        return .{
            .parent = parent,
        };
    }

    fn generate_function(self: *Self, f_decl: *ast.FunctionDeclarationNode) !void {
        _ = self;
        _ = f_decl;
    }
};

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
