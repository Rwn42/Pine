const std = @import("std");

const Token = @import("../frontend/token.zig").Token;
const TokenType = @import("../frontend/token.zig").TokenType;
const Location = @import("../common.zig").Location;
const AST = @import("../frontend/ast.zig");
const typing = @import("typing.zig");
const Stack = @import("../common.zig").Stack;
const Operation = @import("bytecode.zig").Operation;

//TODO: Code re-factor and cleanup
//TODO: compt time code execution
//TODO: Slices or general fat pointer but maybe length is always needed
//TODO: Strings
//TODO: solution for general load store that can work on stack and arbitrary memory address

pub const IRError = error{
    Undeclared,
    TypeMismatch,
    Duplicate,
    OutOfMemory,
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
        parent_func_name: []u8,
    };

    const Self = @This();
    const ScopeStackType = Stack(*Scope, 32, "Too many nested blocks limit is 30");

    program: std.ArrayList(Operation),
    allocator: std.mem.Allocator,
    scope_stack: ScopeStackType,
    tm: typing.TypeManager,
    function_bodies: []*AST.FunctionDeclarationNode,
    functions: std.StringHashMap(usize), //function name to start location
    stack_pointer: usize = 8, //resets to 8 for each function call (compile time address tracking of local vars)

    pub fn init(allocator: std.mem.Allocator, declarations: []AST.Declaration) !Self {
        var tm = typing.TypeManager.init(allocator);
        var bodies = std.ArrayList(*AST.FunctionDeclarationNode).init(allocator);
        for (declarations) |decl| {
            switch (decl) {
                .FunctionDeclaration => |f_decl| {
                    try tm.register_function(f_decl);
                    try bodies.append(f_decl);
                },
                .RecordDeclaration => |r_decl| try tm.register_record(r_decl),
                .ConstantDeclaration => @panic("Not implemented"),
                .ImportDeclaration => @panic("not implemented"),
                //eventually constants go to comptime eval and end up as map of string -> value
            }
        }
        return .{
            .program = std.ArrayList(Operation).init(allocator),
            .function_bodies = try bodies.toOwnedSlice(),
            .allocator = allocator,
            .scope_stack = ScopeStackType.init(),
            .tm = tm,
            .functions = std.StringHashMap(usize).init(allocator),
        };
    }

    pub fn generate_program(self: *Self) ![]Operation {
        defer self.deinit();
        errdefer self.program.deinit();

        //this is to call the main function
        self.program_append(.push, 0);
        self.program_append(.call, null);

        for (self.function_bodies) |f_decl| {
            var scope = Scope{
                .start_ip = self.program.items.len,
                .identifiers = std.StringHashMap(VarInfo).init(self.allocator),
                .parent_func_name = f_decl.name_tk.tag.Identifier,
            };
            self.open_scope(&scope);
            defer self.close_scope(&scope);

            try self.functions.put(f_decl.name_tk.tag.Identifier, scope.start_ip);

            var param_n = f_decl.params;
            while (param_n) |param| {
                const info = try self.register_var_decl(param.name_tk, try self.tm.generate(param.typ));
                self.program_append(.push, info.stack_loc);
                try self.generate_mem_op(info.type_info, false, param.name_tk.loc);
                param_n = param.next;
            }

            for (f_decl.body) |stmt| {
                try StatementGenerator.generate(self, stmt);
            }

            //any other type of function is expected to return properly it will not be auto inserted
            if ((self.tm.functions.get(f_decl.name_tk.tag.Identifier).?).tag == .Void) {
                self.program_append(.ret, null);
            }
        }

        const idx = self.functions.get("main") orelse {
            std.log.err("No main function (main :: fn() {{...}})", .{});
            return IRError.Undeclared;
        };

        self.program.items[0].operand = idx; //replace the push 0 instruction with push addr of main

        return try self.program.toOwnedSlice();
    }

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.scope_stack.get(idx).identifiers.get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("Undeclared identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    fn register_var_decl(self: *Self, name_tk: Token, typ: typing.TypeInfo) !VarInfo {
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

    //generates load/store instructions location given for error reporting
    //assumes that the stack contains all the nessecary parameters
    fn generate_mem_op(self: *Self, type_info: typing.TypeInfo, comptime load: bool, loc: Location) !void {
        switch (type_info.tag) {
            .Void => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .Bool, .Byte, .Integer, .Float, .Pointer => {
                if (load) {
                    self.program_append(if (type_info.size == 1) .load else .load8, null);
                } else {
                    self.program_append(if (type_info.size == 1) .store else .store8, null);
                }
            },
            .UntypedInt => unreachable,
            .Record, .WidePointer => {
                self.program_append(.push, 0); //store the record start in temp memory
                self.program_append(.store8, null); //useful to refer to it for subsequent field load

                const fields = type_info.child.?.field_info.values();
                if (load) std.mem.reverse(typing.FieldInfoStruct, fields);
                for (fields) |field| {
                    self.program_append(.push, 0); //load our start address
                    self.program_append(.load8, null);
                    self.program_append(.push, field.offset);
                    self.program_append(.add_i, 0);
                    try self.generate_mem_op(field.type_info, load, loc);
                }
            },
            .Array => {
                const element_info = type_info.child.?.type_info;
                self.program_append(.push, type_info.size / element_info.size); //length
                if (load) self.program_append(.aload, element_info.size);
                if (!load) self.program_append(.astore, element_info.size);
            },
        }
    }

    fn open_scope(self: *Self, scope: *Scope) void {
        self.stack_pointer = 8;
        self.scope_stack.push(scope);
    }

    fn close_scope(self: *Self, scope: *Scope) void {
        self.scope_stack.pop();
        scope.identifiers.deinit();
    }

    fn program_append(self: *Self, opc: Operation.Opcode, ope: ?u64) void {
        self.program.append(.{ .opc = opc, .operand = ope }) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    fn deinit(self: *Self) void {
        self.allocator.free(self.function_bodies);
        self.tm.deinit();
        self.functions.deinit();
    }
};

pub const StatementGenerator = struct {
    pub fn generate(ir: *IRGenerator, input_stmt: AST.Statement) IRError!void {
        switch (input_stmt) {
            .ReturnStatement => |stmt| {
                const given_type = try ExpressionGenerator.generate_rvalue(ir, stmt);
                const expected_type = ir.tm.functions.get(ir.scope_stack.top().parent_func_name).?;
                try typing.types_equivalent(expected_type, given_type);
                ir.program_append(.ret, null);
            },
            .ExpressionStatement => |expr| {
                const given_type = try ExpressionGenerator.generate_rvalue(ir, expr);
                try typing.types_equivalent(typing.Primitive.get("void").?, given_type);
            },
            .VariableDeclaration => |stmt| {
                const given_type = try ExpressionGenerator.generate_rvalue(ir, stmt.assignment);
                var registered_type = given_type; //type that we will register the var as

                if (stmt.typ) |typ| {
                    registered_type = try ir.tm.generate(typ);
                    try typing.types_equivalent(registered_type, given_type);
                } else {
                    if (registered_type.tag == .UntypedInt) registered_type = typing.Primitive.get("int").?;
                }
                const var_info = try ir.register_var_decl(stmt.name_tk, registered_type);
                ir.program_append(.push, var_info.stack_loc);
                try ir.generate_mem_op(var_info.type_info, false, stmt.name_tk.loc);
            },

            .VariableAssignment => |stmt| {
                const given_info = try ExpressionGenerator.generate_rvalue(ir, stmt.rhs);
                const expected_info = try ExpressionGenerator.generate_lvalue(ir, stmt.lhs);
                try typing.types_equivalent(given_info, expected_info);
                try ir.generate_mem_op(expected_info, false, stmt.loc);
            },

            .IfStatement => try generate_conditional(ir, input_stmt),
            .WhileStatement => try generate_conditional(ir, input_stmt),

            .TemporaryPrint => |expr| {
                const t_info = try ExpressionGenerator.generate_rvalue(ir, expr);
                if (t_info.tag == .Byte) {
                    ir.program_append(.temp_print, 1);
                } else {
                    ir.program_append(.temp_print, 0);
                }
            },
        }
    }

    fn generate_conditional(ir: *IRGenerator, block: AST.Statement) !void {
        const jump_back_ip = ir.program.items.len;

        const condition_info = switch (block) {
            .WhileStatement => |stmt| try ExpressionGenerator.generate_rvalue(ir, stmt.condition),
            .IfStatement => |stmt| try ExpressionGenerator.generate_rvalue(ir, stmt.condition),
            else => unreachable,
        };

        if (condition_info.tag != .Bool) {
            switch (block) {
                .IfStatement => |stmt| std.log.info("Cannot branch on non-boolean type {s}", .{stmt.start_loc}),
                .WhileStatement => |stmt| std.log.info("Cannot branch on non-boolean type {s}", .{stmt.start_loc}),
                else => unreachable,
            }
            return IRError.Syntax;
        }

        var block_scope = IRGenerator.Scope{
            .start_ip = ir.program.items.len,
            .identifiers = std.StringHashMap(VarInfo).init(ir.allocator),
            .parent_func_name = ir.scope_stack.buffer[0].parent_func_name,
        };
        ir.scope_stack.push(&block_scope);
        defer block_scope.identifiers.deinit();
        defer ir.scope_stack.pop();

        ir.program_append(.not, null);
        ir.program_append(.je, 1);
        const body = switch (block) {
            .WhileStatement => |stmt| stmt.body,
            .IfStatement => |stmt| stmt.body,
            else => unreachable,
        };
        for (body) |body_stmt| {
            try StatementGenerator.generate(ir, body_stmt);
        }

        switch (block) {
            .WhileStatement => ir.program_append(.jmp, jump_back_ip),
            else => {},
        }

        ir.program.items[block_scope.start_ip + 1].operand = ir.program.items.len; //plus 1 skips the not inst
    }
};

const ExpressionGenerator = struct {
    pub fn generate_rvalue(ir: *IRGenerator, input_expr: AST.Expression) IRError!typing.TypeInfo {
        switch (input_expr) {
            .LiteralInt => |token| {
                ir.program_append(.push, @bitCast(token.tag.Integer));
                return typing.Primitive.get("untyped_int").?;
            },
            .LiteralFloat => |token| {
                ir.program_append(.push, @bitCast(token.tag.Float));
                return typing.Primitive.get("float").?;
            },
            .LiteralBool => |token| {
                ir.program_append(.push, if (token.tag == .True) 1 else 0);
                return typing.Primitive.get("bool").?;
            },
            .FunctionInvokation => |expr| {
                var list = reverse_expr_list(expr.args_list);
                while (list) |list_node| {
                    _ = try ExpressionGenerator.generate_rvalue(ir, list_node.expr);
                    list = list_node.next;
                }
                const idx = ir.functions.get(expr.name_tk.tag.Identifier) orelse {
                    std.log.err("Cannot find function {s}", .{expr.name_tk});
                    return IRError.Undeclared;
                };
                ir.program_append(.push, idx);
                ir.program_append(.call, ir.program.items.len + 1);

                return ir.tm.functions.get(expr.name_tk.tag.Identifier).?;
            },
            .BinaryExpression => |expr| {
                _ = try ExpressionGenerator.generate_rvalue(ir, expr.lhs);
                const ti = try ExpressionGenerator.generate_rvalue(ir, expr.rhs);

                switch (expr.op.tag) {
                    .Plus => ir.program_append(.add_i, 0),
                    .Dash => ir.program_append(.add_i, 1),
                    .Asterisk => ir.program_append(.mul_i, 0),
                    .SlashForward => ir.program_append(.mul_i, 1),
                    .DoubleEqual => ir.program_append(.eq, 0),
                    .NotEqual => ir.program_append(.eq, 1),
                    .LessThan => ir.program_append(.lt_i, 0),
                    .LessThanEqual => ir.program_append(.lte_i, 0),
                    .GreaterThan => ir.program_append(.lt_i, 1),
                    .GreaterThanEqual => ir.program_append(.lte_i, 1),

                    else => unreachable,
                }

                switch (expr.op.tag) {
                    .DoubleEqual, .NotEqual, .LessThan, .LessThanEqual, .GreaterThan, .GreaterThanEqual => {
                        return typing.Primitive.get("bool").?;
                    },
                    else => {},
                }
                return ti;
            },
            .UnaryExpression => |expr| {
                switch (expr.op.tag) {
                    .Ampersand => {
                        return try ExpressionGenerator.generate_lvalue(ir, expr.expr);
                    },
                    .Hat => {
                        const ti_info = try ExpressionGenerator.generate_rvalue(ir, expr.expr);
                        if (ti_info.tag != .Pointer) {
                            std.log.err("Cannot dereference a non pointer {s}", .{expr.op});
                            return IRError.Syntax;
                        }
                        try ir.generate_mem_op(ti_info.child.?.type_info.*, true, expr.op.loc);
                        return ti_info.child.?.type_info.*;
                    },
                    .ExclamationMark => {
                        _ = try ExpressionGenerator.generate_rvalue(ir, expr.expr);
                        ir.program_append(.not, null);
                        return typing.Primitive.get("bool").?;
                    },
                    .Dash => {
                        const ti_info = try ExpressionGenerator.generate_rvalue(ir, expr.expr);
                        const multiplier: i64 = -1;
                        ir.program_append(.push, @bitCast(multiplier));
                        if (ti_info.tag == .Float) {
                            ir.program_append(.mul_f, 0);
                        } else {
                            ir.program_append(.mul_i, 0);
                        }
                        return ti_info;
                    },
                    else => unreachable,
                }
            },
            .IdentifierInvokation => |token| {
                const info = try ir.find_identifier(token, ir.scope_stack.top_idx());
                ir.program_append(.push, info.stack_loc);
                try ir.generate_mem_op(info.type_info, true, token.loc);
                return info.type_info;
            },

            .ArrayInitialization => |ordered_list| {
                const list = reverse_expr_list(ordered_list);
                var length: usize = 0;
                var list_o: ?*AST.ExprList = list;
                var element_type = ir.tm.new_info();
                while (list_o) |list_node| {
                    length += 1;
                    element_type.* = try generate_rvalue(ir, list_node.expr);
                    list_o = list_node.next;
                }
                return .{
                    .tag = .Array,
                    .size = length * element_type.size,
                    .child = .{ .type_info = element_type },
                };
            },
            .RecordInitialization => |list| {
                const typ = ir.tm.records.get(list.name_tk.tag.Identifier) orelse {
                    std.log.err("Record type {s} is undefined {s}", .{ list.name_tk.tag.Identifier, list.name_tk.loc });
                    return IRError.Undeclared;
                };

                //this is awfully slow
                std.mem.reverse([]const u8, typ.child.?.field_info.keys());
                outer: for (typ.child.?.field_info.keys()) |field_name| {
                    var node_o: ?*AST.FieldList = list.fields;
                    while (node_o) |ast_field| {
                        if (std.mem.eql(u8, ast_field.field.tag.Identifier, field_name)) {
                            //could do some type checking here
                            _ = try ExpressionGenerator.generate_rvalue(ir, ast_field.expr);
                            continue :outer;
                        }
                        node_o = ast_field.next;
                    }
                    std.log.err("Did not initialize field {s} {s}", .{ field_name, list.name_tk.loc });
                    return IRError.Syntax;
                }
                std.mem.reverse([]const u8, typ.child.?.field_info.keys());
                return typ;
            },
            .Cast => |expr| {
                _ = try ExpressionGenerator.generate_rvalue(ir, expr.expr);
                return ir.tm.generate(expr.destination_type);
            },
            .AccessExpression => |expr| {
                const info = try ExpressionGenerator.generate_lvalue(ir, input_expr);
                try ir.generate_mem_op(info, true, expr.op.loc);
                return info;
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
                const type_info = try ExpressionGenerator.generate_access(ir, expr.rhs, initial_info);
                ir.program_append(.add_i, 0);
                return type_info;
            },
            .UnaryExpression => |expr| {
                if (expr.op.tag != .Hat) {
                    std.log.err("Cannot assign to a non variable {s}", .{expr.op.loc});
                    return IRError.Syntax;
                }
                return try ExpressionGenerator.generate_rvalue(ir, expr.expr);
            },
            else => {
                std.log.err("{s} is not an lvalue (cannot be assigned to)", .{input_expr});
                return IRError.Syntax;
            },
        }
    }

    fn generate_access(ir: *IRGenerator, expr: AST.Expression, input_info: typing.TypeInfo) !typing.TypeInfo {
        switch (expr) {
            .IdentifierInvokation => |field_tk| {
                switch (input_info.tag) {
                    .Array => {
                        _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                        ir.program_append(.push, input_info.child.?.type_info.size);
                        ir.program_append(.mul_i, 0);
                        return input_info.child.?.type_info.*;
                    },
                    .Record, .WidePointer => {
                        if (input_info.child.?.field_info.get(field_tk.tag.Identifier)) |field_info| {
                            ir.program_append(.push, field_info.offset);
                            return field_info.type_info;
                        }
                        std.log.err("No field {s} in record", .{field_tk});
                        return IRError.Syntax;
                    },
                    else => {
                        std.log.err("Cannot access variable (. operator) if it is not a record or array {s}", .{field_tk});
                        return IRError.Syntax;
                    },
                }
            },
            .AccessExpression => |a_expr| {
                switch (input_info.tag) {
                    .Record => {
                        const rhs_input_info = try generate_access(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access(ir, a_expr.rhs, rhs_input_info);
                        ir.program_append(.add_i, 0);
                        return rhs_info;
                    },
                    .Array => {
                        _ = try generate_access(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access(ir, a_expr.rhs, input_info.child.?.type_info.*);
                        ir.program_append(.add_i, 0);
                        return rhs_info;
                    },
                    else => {
                        std.log.err("Cannot access variable (. operator) if it is not a record or array {s}", .{expr});
                        return IRError.Syntax;
                    },
                }
            },
            //some sort of array access expression
            else => {
                if (input_info.tag != .Array) {
                    std.log.err("Tried to access a non indexible value {s}", .{expr});
                    return IRError.Syntax;
                }
                _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                ir.program_append(.push, input_info.child.?.type_info.size);
                ir.program_append(.mul_i, 0);
                return input_info.child.?.type_info.*;
            },
        }
    }
};

//reverse the linked list AST should probably just change but works for now.
fn reverse_expr_list(list: ?*AST.ExprList) ?*AST.ExprList {
    var current: ?*AST.ExprList = list;
    var prev: ?*AST.ExprList = null;
    var next: ?*AST.ExprList = null;

    while (current) |curr_node| {
        next = curr_node.next;
        curr_node.next = prev;
        prev = current;
        current = next;
    }
    return prev;
}
