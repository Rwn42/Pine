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
    const ScopeStackType = Stack(Scope, 32, "Too many nested blocks limit is 30");

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
            self.scope_stack.push(function_scope);
            defer self.scope_stack.pop();
            defer function_scope.identifiers.deinit();

            var param_n = f_decl.params;
            while (param_n) |param| {
                const info = try self.register_var_decl(param.name_tk, param.typ);
                try self.generate_stack_op(info.type_info, false, param.name_tk.loc);
                param_n = param.next;
            }

            for (f_decl.body) |stmt| {
                try StatementGenerator.generate(self, stmt);
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
        self.stack_pointer += typ.size / 8;

        return self.scope_stack.top().identifiers.get(name_tk.tag.Identifier).?;
    }

    //NOTE: assume params are placed on the stack in reverse order
    //f(1, 2, 3) the stack is then 3 2 1 on operand stack
    fn generate_stack_op(self: *Self, type_info: typing.TypeInfo, load: bool, loc: Location) !void {
        if (type_info.tag == .Void) {
            std.log.err("Cannot have variable of type void {s}", .{loc});
            return IRError.Syntax;
        }

        //some sort of primitive including pointers
        if (type_info.tag != .Record and type_info.tag != .Array) {
            self.program_append(.push, self.stack_pointer);
            if (load) {
                self.program_append(if (type_info.size == 1) .load else .load8, null);
            } else {
                self.program_append(if (type_info.size == 1) .store else .store8, null);
            }
            self.stack_pointer += type_info.size;
            return;
        }

        if (type_info.tag == .Array) {
            const element_type = type_info.child.?.type_info;
            const length = type_info.size / element_type.size;
            for (0..length) |_| try self.generate_stack_op(element_type.*, load, loc);
            return;
        }

        if (type_info.tag != .Record) @panic("New type tag update generate stack op");

        var vals = type_info.child.?.field_info.values();
        for (vals) |field| try self.generate_stack_op(field.type_info, load, loc);
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
        _ = ir; // autofix
        _ = input_stmt; // autofix

        return IRError.Duplicate;
    }
};

const ExpressionGenerator = struct {
    fn generate_rvalue(ir: *IRGenerator, input_expr: AST.Expression) !void {
        _ = ir; // autofix
        _ = input_expr; // autofix

        return IRError.Duplicate;
    }

    fn generate_lvalue(ir: *IRGenerator, input_expr: AST.Expression) !void {
        _ = ir; // autofix
        _ = input_expr; // autofix

        return IRError.Duplicate;
    }
};
