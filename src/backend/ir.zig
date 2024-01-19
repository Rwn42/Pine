const std = @import("std");

const Token = @import("../frontend/token.zig").Token;
const TokenType = @import("../frontend/token.zig").TokenType;
const Location = @import("../common.zig").Location;
const AST = @import("../frontend/ast.zig");
const typing = @import("typing.zig");
const Stack = @import("../common.zig").Stack;
const Operation = @import("bytecode.zig").Operation;

pub const IRError = error{
    Undeclared,
    Mismatch,
    Duplicate,
    Syntax,
};

const VarInfo = struct {
    stack_loc: usize,
    type_info: typing.TypeInfo,
};

pub const IRGenerator = struct {
    const Scope = struct {
        identifiers: std.StringHashMap(VarInfo),
        start_ip: usize,
    };

    const Self = @This();
    const ScopeStackType = Stack(*Scope, 32, "Too many nested blocks limit is 30");

    program: std.ArrayList(Operation),
    allocator: std.mem.Allocator,
    scope_stack: ScopeStackType,
    tm: typing.TypeManager,
    function_bodies: []*AST.FunctionDeclarationNode,
    stack_pointer: usize,

    pub fn init(allocator: std.mem.Allocator, declarations: []AST.Declaration) !Self {
        var tm = typing.TypeManager.init(allocator);
        var bodies = std.ArrayList(*AST.FunctionDeclarationNode).init(allocator);
        for (declarations) |decl| {
            switch (decl) {
                .FunctionDeclaration => |f_decl| {
                    try tm.register_function(f_decl);
                    bodies.append(f_decl) catch {
                        @panic("FATAL COMPILER ERROR: Out of memory");
                    };
                },
                .RecordDeclaration => |r_decl| try tm.register_record(r_decl),
                .ConstantDeclaration => @panic("Not implemented"),
                //eventually constants go to comptime eval and end up as map of string -> value
            }
        }
        return .{
            .program = std.ArrayList(Operation).init(allocator),
            .function_bodies = bodies.toOwnedSlice() catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            },
            .allocator = allocator,
            .scope_stack = ScopeStackType.init(),
            .tm = tm,
            .stack_pointer = 0,
        };
    }

    pub fn generate_program(self: *Self) ![]Operation {
        defer self.deinit();
        for (self.function_bodies) |f_decl| {
            var function_scope = Scope{
                .start_ip = self.program.items.len,
                .identifiers = std.StringHashMap(VarInfo).init(self.allocator),
            };
            self.stack_pointer = 0;
            self.scope_stack.push(&function_scope);
            defer function_scope.identifiers.deinit();
            defer self.scope_stack.pop();

            var param_n = f_decl.params;
            while (param_n) |param| {
                const info = try self.register_var_decl(param.name_tk, param.typ);
                try self.generate_stack_store(info, param.name_tk.loc);
                param_n = param.next;
            }

            for (f_decl.body) |stmt| {
                StatementGenerator.generate(self, stmt) catch {};
            }
        }
        return try self.program.toOwnedSlice();
    }

    //does not generate any instructions just adds it to the scope
    fn register_var_decl(self: *Self, name_tk: Token, dt: AST.DefinedType) !VarInfo {
        const typ = try self.tm.generate(dt);
        if (self.scope_stack.top().identifiers.contains(name_tk.tag.Identifier)) {
            std.log.err("Duplicate definition of identifier {s}", .{name_tk});
            return IRError.Duplicate;
        }
        try self.scope_stack.top().identifiers.put(name_tk.tag.Identifier, .{
            .type_info = typ,
            .stack_loc = self.stack_pointer,
        });
        self.stack_pointer += typ.size;

        return self.scope_stack.top().identifiers.get(name_tk.tag.Identifier).?;
    }

    //NOTE: assume params are placed on the stack in reverse order
    //f(1, 2, 3) the stack is then 3 2 1 on operand stack
    fn generate_stack_store(self: *Self, info: VarInfo, loc: Location) !void {
        const type_info = info.type_info;

        switch (type_info.tag) {
            .Void => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .Bool, .Byte, .Integer, .Float, .Pointer => {
                self.program_append(.push, info.stack_loc);
                self.program_append(if (type_info.size == 1) .store else .store8, info.stack_loc);
            },
            .Array => {
                const element_type = type_info.child.?.type_info;
                const length = type_info.size / element_type.size;
                for (0..length) |i| {
                    try self.generate_stack_store(.{ .type_info = element_type.*, .stack_loc = i * element_type.size }, loc);
                }
            },
            .Record => {
                for (info.type_info.child.?.field_info.values()) |field| {
                    try self.generate_stack_store(.{ .type_info = field.type_info, .stack_loc = field.offset + info.stack_loc }, loc);
                }
                return;
            },
            else => @panic("cannot yet store type"),
        }
    }

    fn program_append(self: *Self, opc: Operation.Opcode, ope: ?u64) void {
        self.program.append(.{ .opc = opc, .operand = ope }) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }
    fn deinit(self: *Self) void {
        self.allocator.free(self.function_bodies);
        self.tm.deinit();
    }
};

const StatementGenerator = struct {
    fn generate(ir: *IRGenerator, input_stmt: AST.Statement) !void {
        switch (input_stmt) {
            .ReturnStatement => |stmt| {
                try ExpressionGenerator.generate_rvalue(ir, stmt);
                ir.program_append(.ret, null);
            },
            else => {},
        }
    }
};

const ExpressionGenerator = struct {
    fn generate_rvalue(ir: *IRGenerator, input_expr: AST.Expression) !void {
        switch (input_expr) {
            .LiteralInt => |token| {
                ir.program_append(.push, @bitCast(token.tag.Integer));
            },
            else => {},
        }
    }

    fn generate_lvalue(ir: *IRGenerator, input_expr: AST.Expression) !void {
        _ = ir; // autofix
        _ = input_expr; // autofix

        return IRError.Duplicate;
    }
};
