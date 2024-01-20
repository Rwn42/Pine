const std = @import("std");

const Token = @import("../frontend/token.zig").Token;
const TokenType = @import("../frontend/token.zig").TokenType;
const Location = @import("../common.zig").Location;
const AST = @import("../frontend/ast.zig");
const typing = @import("typing.zig");
const Stack = @import("../common.zig").Stack;
const Operation = @import("bytecode.zig").Operation;

//TODO: Code cleanup
//TODO: implement array / record initilization remember to reverse order
//TODO: little interpreter for testing

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
                self.program_append(.push, info.stack_loc);
                try self.generate_stack_store(info.type_info, param.name_tk.loc);
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

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.scope_stack.get(idx).identifiers.get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("No variable declared for identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    //NOTE: assume params are placed on the stack in reverse order
    //f(1, 2, 3) the stack is then 3 2 1 on operand stack
    //NOTE: assume address to store to is also on the stack so f(1, 2, 3) 3 2 1 0 to store 1 at 0.
    fn generate_stack_store(self: *Self, type_info: typing.TypeInfo, loc: Location) !void {
        switch (type_info.tag) {
            .Void => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .Bool, .Byte, .Integer, .Float, .Pointer => {
                self.program_append(if (type_info.size == 1) .store else .store8, null);
            },
            .Array => {
                self.program_append(.gstore, null); //store the address into the gp reg
                const element_type = (type_info.child orelse @panic("compiler error")).type_info;
                const length = type_info.size / element_type.size;
                for (0..length) |i| {
                    self.program_append(.gload, null);
                    self.program_append(.push, element_type.size);
                    self.program_append(.push, i);
                    self.program_append(.mul_i, null);
                    self.program_append(.add_i, null);
                    try self.generate_stack_store(element_type.*, loc);
                }
            },
            .Record => {
                self.program_append(.gstore, null); //store the address into the gp reg
                for (type_info.child.?.field_info.values()) |field| {
                    self.program_append(.gload, null);
                    self.program_append(.push, field.offset);
                    self.program_append(.add_i, null);
                    try self.generate_stack_store(field.type_info, loc);
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
                _ = try ExpressionGenerator.generate_rvalue(ir, stmt);
                ir.program_append(.ret, null);
            },
            .VariableDeclaration => |stmt| {
                if (stmt.typ == null) @panic("variable type inference not implemented");
                const var_info = try ir.register_var_decl(stmt.name_tk, stmt.typ.?);
                if (stmt.assignment) |assignment| {
                    _ = try ExpressionGenerator.generate_rvalue(ir, assignment);
                    ir.program_append(.push, var_info.stack_loc);
                    try ir.generate_stack_store(var_info.type_info, stmt.name_tk.loc);
                }
            },
            .VariableAssignment => |stmt| {
                _ = try ExpressionGenerator.generate_rvalue(ir, stmt.rhs);
                const type_info = try ExpressionGenerator.generate_lvalue(ir, stmt.lhs);
                try ir.generate_stack_store(type_info, stmt.loc);
            },
            else => {},
        }
    }
};

const ExpressionGenerator = struct {
    fn generate_rvalue(ir: *IRGenerator, input_expr: AST.Expression) !typing.TypeInfo {
        switch (input_expr) {
            .LiteralInt => |token| {
                ir.program_append(.push, @bitCast(token.tag.Integer));
                return typing.Primitive.get("int").?;
            },
            .IdentifierInvokation => |token| {
                const info = try ir.find_identifier(token, ir.scope_stack.top_idx());
                ir.program_append(.push, info.stack_loc);
                ir.program_append(.load, null);
                return info.type_info;
            },
            .ArrayInitialization => |list| {
                //arrays need to be placed in reverse order
                //so save start location
                var start_ip = ir.program.items.len;
                var list_o: ?*AST.ExprList = list;
                var element_type = ir.tm.new_info();

                while (list_o) |list_node| {
                    element_type.* = try generate_rvalue(ir, list_node.expr);
                    list_o = list_node.next;
                }
                std.mem.reverse(Operation, ir.program.items[start_ip..]);
                return .{
                    .tag = .Array,
                    .size = ir.program.items[start_ip..].len * element_type.size,
                    .child = .{ .type_info = element_type },
                };
            },
            else => @panic("Not implemented"),
        }
    }

    fn generate_lvalue(ir: *IRGenerator, input_expr: AST.Expression) !typing.TypeInfo {
        switch (input_expr) {
            .IdentifierInvokation => |token| {
                const var_info = try ir.find_identifier(token, ir.scope_stack.top_idx());
                ir.program_append(.push, var_info.stack_loc);
                return var_info.type_info;
            },
            .AccessExpression => |expr| {
                const initial_info = try ExpressionGenerator.generate_lvalue(ir, expr.lhs);
                const type_info = try ExpressionGenerator.generate_access_r(ir, expr.rhs, initial_info);
                ir.program_append(.add_i, null);
                return type_info;
            },
            .UnaryExpression => |expr| {
                if (expr.op.tag != .Hat) {
                    std.log.err("Cannot assign to a non variable {s}", .{expr.op.loc});
                    return IRError.Syntax;
                }
                return try ExpressionGenerator.generate_rvalue(ir, expr.expr);
            },
            else => @panic("not implemented"),
        }
    }

    fn generate_access_r(ir: *IRGenerator, expr: AST.Expression, input_info: typing.TypeInfo) !typing.TypeInfo {
        switch (expr) {
            .IdentifierInvokation => |field_tk| {
                if (input_info.tag != .Record) {
                    std.log.err("cannot access a non record variable {s}", .{field_tk});
                    return IRError.Syntax;
                }
                if (input_info.child.?.field_info.get(field_tk.tag.Identifier)) |field_info| {
                    ir.program_append(.push, field_info.offset);
                    return field_info.type_info;
                }
                std.log.err("No field {s} in record", .{field_tk});
                return IRError.Syntax;
            },
            .AccessExpression => |a_expr| {
                switch (input_info.tag) {
                    .Record => {
                        const rhs_input_info = try generate_access_r(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access_r(ir, a_expr.rhs, rhs_input_info);
                        ir.program_append(.add_i, null);
                        return rhs_info;
                    },
                    .Array => {
                        _ = try generate_access_r(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access_r(ir, a_expr.rhs, input_info.child.?.type_info.*);
                        ir.program_append(.add_i, null);
                        return rhs_info;
                    },
                    else => {
                        std.log.err("Must access record or array {s}", .{a_expr.op.loc});
                        return IRError.Syntax;
                    },
                }
            },
            //some sort of array access
            else => {
                if (input_info.child == null) {
                    //TODO: dont print the whole expression or maybe...
                    std.log.err("Tried to access a non indexible value {s}", .{expr});
                    return IRError.Syntax;
                }
                _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                ir.program_append(.push, input_info.child.?.type_info.size);
                ir.program_append(.mul_i, null);
                return input_info.child.?.type_info.*;
            },
        }
    }
};
