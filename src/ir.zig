const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");
const typing = @import("typing.zig");

// ----IR Plan probably not smart----
//each stack needs a map of strings to integers where the integer is where in the stack memory the variable lies
// stack_mem: []u8 -> {0, 0 ,0 ...}
// a: byte = 10 -> {10, 0, 0} map location is 0
//some_struct: SomeStruct = {x: 11, y: 12}; -> {10, 11, 12} some_struct at 1
//some_struct.x 1 + offset of x

const Opcode = enum {
    s_load, //operand is load size
    s_store, //operand is store size
    i_add, //operand is for inverse (-)
    f_add,
    i_mul, //operand is for inverse (/)
    f_mul,
    call,
    ret,
    push, //operand is what value to push
};

const Operand = union(enum) {
    integer: i64,
    float: f64,
};

const Operation = struct {
    opc: Opcode,
    operand: Operand,

    pub fn format(self: Operation, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} ", .{@tagName(self.opc)});
        switch (self.operand) {
            .integer => |v| try writer.print("{d}", .{v}),
            .float => |v| try writer.print("{d}", .{v}),
        }
    }
};

fn Stack(comptime T: type, comptime limit: usize) type {
    return struct {
        const Self = @This();

        sp: usize,
        buffer: [limit]T,

        pub fn init() Self {
            return .{
                .sp = 0,
                .buffer = undefined,
            };
        }

        fn push(self: *Self, value: T) void {
            if (self.sp >= limit - 1) {
                @panic("FATAL COMPILER ERROR: Stack Overflow");
            }
            self.buffer[self.sp] = value;
            self.sp += 1;
        }

        fn pop(self: *Self) void {
            if (self.sp <= 0) {
                @panic("FATAL COMPILER ERROR: Stack Underflow");
            }
            self.sp -= 1;
        }

        fn top(self: *Self) T {
            return self.buffer[self.sp - 1];
        }

        fn get(self: *Self, idx: usize) T {
            if (idx < 0 or idx >= limit) @panic("Out of bounds access");
            return self.buffer[idx];
        }
    };
}

const IdInfo = struct {
    type_info: typing.TypeInfo, //might be better to do a pointer here (mem usage)
    stack_loc: isize,
};

const ScopeT = std.StringHashMap(IdInfo);
const Scope = *std.StringHashMap(IdInfo);

const IRError = error{
    DuplicateDefinition,
    Undeclared,
};

pub const IRGenerator = struct {
    const Self = @This();

    sp: isize, //stack memory pointer will reset to 0 for each different func call
    scopes: Stack(Scope, 32), //global scope + function scope + 30 nested blocks seems good enough
    declarations: []AST.Declaration,
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    program: std.ArrayList(Operation),

    pub fn init(declarations: []AST.Declaration, allocator: std.mem.Allocator) Self {
        return .{
            .sp = 0,
            .scopes = Stack(Scope, 32).init(),
            .declarations = declarations,
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .program = std.ArrayList(Operation).init(allocator),
        };
    }

    pub fn generate(self: *Self) !void {
        for (self.declarations) |decl_type| {
            switch (decl_type) {
                .FunctionDeclaration => |fndecl| {
                    //TODO: register its type
                    self.sp = 0;
                    var function_scope = ScopeT.init(self.allocator);
                    self.scopes.push(&function_scope);

                    for (fndecl.body) |stmt| {
                        try self.compile_stmt(stmt);
                    }

                    self.scopes.pop();
                    function_scope.deinit();
                },
                else => {
                    @panic("Not Implemented");
                },
            }
        }
    }

    fn compile_stmt(self: *Self, stmt: AST.Statement) !void {
        switch (stmt) {
            .ExpressionStatement => |expr| try self.compile_expression(expr),
            .ReturnStatement => |expr| {
                try self.compile_expression(expr);
                self.program_append(.ret, .{ .integer = 0 });
            },
            .VariableDeclaration => |decl| {
                const name = decl.name_tk.tag.Identifier;
                if (self.scopes.top().contains(name)) {
                    std.log.err("Duplicate defintion of variable {s}", .{decl.name_tk});
                    return IRError.DuplicateDefinition;
                }
                const typ = try typing.TypeInfo.gen(decl.typ.?); //TODO: type inference
                self.scopes.top().put(name, .{ .type_info = typ, .stack_loc = self.sp }) catch {
                    @panic("FATAL COMPILER ERROR: Out of memory");
                };
                self.sp += 1;

                if (decl.assignment) |assignment| {
                    try self.compile_expression(assignment); //assignment expression
                    self.program_append(.push, .{ .integer = self.sp - 1 }); //addr of var
                    self.program_append(.s_store, .{ .integer = @intCast(typ.size) }); //store the var
                }
            },
            .VariableAssignment => |assignment| {
                const name = assignment.name_tk.tag.Identifier;
                const info = self.find_info(name, self.scopes.sp - 1) catch |err| {
                    std.log.err("Undeclared Identifier {s}", .{assignment.name_tk});
                    return err;
                };
                try self.compile_expression(assignment.assignment);
                self.program_append(.push, .{ .integer = info.stack_loc });
                self.program_append(.s_store, .{ .integer = @intCast(info.type_info.size) });
            },
            else => @panic("not implemented"),
        }
    }

    //traverse through open scopes to find identifier
    fn find_info(self: *Self, id: []u8, idx: usize) !IdInfo {
        if (self.scopes.get(idx).get(id)) |info| return info;
        if (idx == 0) return IRError.Undeclared;
        return try self.find_info(id, idx - 1);
    }

    fn compile_expression(self: *Self, expr: AST.Expression) !void {
        switch (expr) {
            .LiteralInt => |tk| self.program_append(.push, .{ .integer = tk.tag.Integer }),
            .LiteralBool => |tk| switch (tk.tag) {
                .True => self.program_append(.push, .{ .integer = 1 }),
                .False => self.program_append(.push, .{ .integer = 0 }),
                else => unreachable,
            },
            .LiteralFloat => |tk| self.program_append(.push, .{ .float = tk.tag.Float }),
            .LiteralString => @panic("Not Implemented"),
            .IdentifierInvokation => |tk| {
                const name = tk.tag.Identifier;
                const info = self.find_info(name, self.scopes.sp - 1) catch |err| {
                    std.log.err("Undeclared Identifier {s}", .{tk});
                    return err;
                };
                self.program_append(.push, .{ .integer = info.stack_loc });
                self.program_append(.s_load, .{ .integer = @intCast(info.type_info.size) });
            },
            else => @panic("Not implemented"),
        }
    }

    fn program_append(self: *Self, opc: Opcode, ope: Operand) void {
        self.program.append(.{ .opc = opc, .operand = ope }) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self; // autofix
    }
};
