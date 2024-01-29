const std = @import("std");

const Token = @import("../frontend/token.zig").Token;
const TokenType = @import("../frontend/token.zig").TokenType;
const Location = @import("../common.zig").Location;
const AST = @import("../frontend/ast.zig");
const typing = @import("typing.zig");
const Stack = @import("../common.zig").Stack;
const Operation = @import("bytecode.zig").Operation;
const Interpreter = @import("interpreter.zig").Interpreter;

pub const IRError = error{
    Undeclared,
    TypeMismatch,
    Duplicate,
    OutOfMemory,
    Syntax,
};

const Program = std.ArrayList(Operation);
const IdMap = std.StringHashMap(VarInfo);

const VarInfo = struct {
    ct_known: bool,
    stack_addr: usize,
    type_info: typing.TypeInfo,
};
const Block = struct {
    identifiers: IdMap,
    start_ip: usize,
    parent_func_name: ?[]u8,
};

const ScopeStack = Stack(*Block, 30, "Too many nested blocks!");

//specifically for generating IR from AST from main file
pub fn generate_main(allocator: std.mem.Allocator, ast_roots: []AST.Declaration) ![]Operation {
    var state = IRState.init(allocator);
    state.active_buffer = &state.program;
    defer state.deinit();

    //instructions for calling main
    IRState.program_append(&state, .push, 0);
    IRState.program_append(&state, .call, null);

    var program = try generate(allocator, &state, ast_roots);

    //edit main call instructions with main ip
    program[0].operand = state.function_ips.get("main") orelse {
        std.log.err("No main function defined main :: fn() void {{...}}", .{});
        return IRError.Undeclared;
    };

    return program;
}

pub fn generate(allocator: std.mem.Allocator, input_state: ?*IRState, ast_roots: []AST.Declaration) ![]Operation {
    var defualt_state = IRState.init(allocator);
    defer defualt_state.deinit();
    var state = input_state orelse &defualt_state;
    state.active_buffer = &state.program;
    state.ct_interpreter.open_scratch_scope();
    state.tm.state = state;
    defer allocator.destroy(state.ct_interpreter.call_stack.pop_ret());

    var global_scope = Block{ .start_ip = 0, .identifiers = IdMap.init(allocator), .parent_func_name = null };
    state.scope.push(&global_scope);
    defer state.close_scope();

    for (ast_roots) |root| {
        try DeclarationGenerator.generate(state, root);
    }

    return state.program.toOwnedSlice();
}

pub const IRState = struct {
    const Self = @This();

    program: Program,
    ct_buffer: Program,
    active_buffer: *Program,
    ct_interpreter: Interpreter,
    scope: ScopeStack = ScopeStack.init(),
    function_ips: std.StringHashMap(usize),
    allocator: std.mem.Allocator,
    tm: typing.TypeManager,
    stack_pointer: usize = 8, //compile time addr tracking

    fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .program = Program.init(allocator),
            .active_buffer = undefined,
            .ct_buffer = Program.init(allocator),
            .ct_interpreter = Interpreter.init_blank(allocator),
            .function_ips = std.StringHashMap(usize).init(allocator),
            .tm = typing.TypeManager.init(allocator),
            .allocator = allocator,
        };
    }

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.scope.get(idx).identifiers.get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("Undeclared identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    fn register_var_decl(self: *Self, name_tk: Token, typ: typing.TypeInfo) !VarInfo {
        if (self.scope.top().identifiers.contains(name_tk.tag.Identifier)) {
            std.log.err("Duplicate definition of identifier {s}", .{name_tk});
            return IRError.Duplicate;
        }
        try self.scope.top().identifiers.put(name_tk.tag.Identifier, .{
            .type_info = typ,
            .ct_known = if (self.scope.top_idx() == 0) true else false,
            .stack_addr = self.stack_pointer,
        });
        self.stack_pointer += typ.size;
        return self.scope.top().identifiers.get(name_tk.tag.Identifier).?;
    }

    //generates load/store instructions location given for error reporting
    //assumes that the stack contains all the nessecary parameters
    fn generate_mem_op(self: *Self, type_info: typing.TypeInfo, comptime load: bool, loc: Location) !void {
        switch (type_info.tag) {
            .Void => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .Bool, .Byte, .Integer, .Float, .Pointer, .UntypedInt => {
                if (load) {
                    self.program_append(if (type_info.size == 1) .load else .load8, null);
                } else {
                    self.program_append(if (type_info.size == 1) .store else .store8, null);
                }
            },
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

    pub fn enter_ct_mode(self: *Self) void {
        self.active_buffer = &self.ct_buffer;
    }

    //returns whats left on the operand stack after execution
    //useful for comptime expression execution such as for array length
    pub fn execute_ct_buffer(self: *Self) ![]u64 {
        const program = try self.ct_buffer.toOwnedSlice();
        defer self.allocator.free(program);
        self.ct_interpreter.load_new_program(program);
        self.ct_interpreter.run() catch {};
        return self.ct_interpreter.operand_stack.buffer[0..self.ct_interpreter.operand_stack.sp];
    }

    pub fn exit_ct_mode(self: *Self) void {
        self.active_buffer = &self.program;
    }

    fn program_append(self: *Self, opc: Operation.Opcode, operand: ?u64) void {
        self.active_buffer.*.append(.{ .opc = opc, .operand = operand }) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    fn program_len(self: *Self) usize {
        return self.active_buffer.items.len;
    }

    fn close_scope(self: *Self) void {
        const block = self.scope.pop_ret();
        block.identifiers.deinit();
    }

    fn deinit(self: *Self) void {
        self.program.deinit();
        self.ct_buffer.deinit();
        self.tm.deinit();
        self.function_ips.deinit();
    }
};

const DeclarationGenerator = struct {
    fn generate(state: *IRState, decl: AST.Declaration) !void {
        switch (decl) {
            .RecordDeclaration => |r_decl| try state.tm.register_record(r_decl),
            .FunctionDeclaration => |f_decl| {
                state.stack_pointer = 8; //new function so reset sp to 8

                const name = f_decl.name_tk.tag.Identifier;
                try state.tm.register_function(f_decl);
                try state.function_ips.put(name, state.program_len());

                var function_scope = Block{
                    .parent_func_name = name,
                    .start_ip = state.program_len(),
                    .identifiers = IdMap.init(state.allocator),
                };
                state.scope.push(&function_scope);
                defer state.close_scope();

                var param_n = f_decl.params;
                while (param_n) |param| {
                    const info = try state.register_var_decl(param.name_tk, try state.tm.generate(param.typ));
                    state.program_append(.push, info.stack_addr);
                    try state.generate_mem_op(info.type_info, false, param.name_tk.loc);
                    param_n = param.next;
                }

                for (f_decl.body) |stmt| {
                    try StatementGenerator.generate(state, stmt);
                }

                //auto inserts return for void functions
                //any other type of function is expected to return properly it will not be auto inserted
                if ((state.tm.functions.get(f_decl.name_tk.tag.Identifier).?).tag == .Void) {
                    state.program_append(.ret, null);
                }
            },
            .ConstantDeclaration => |c_decl| {
                state.enter_ct_mode();
                defer state.exit_ct_mode();

                const type_info = try ExpressionGenerator.generate_rvalue(state, c_decl.value);
                const var_info = try state.register_var_decl(c_decl.name_tk, type_info);
                state.program_append(.push, var_info.stack_addr);
                try state.generate_mem_op(type_info, false, c_decl.name_tk.loc);
                _ = try state.execute_ct_buffer();
            },
            .ImportDeclaration => @panic("not implemented"),
        }
    }
};

pub const StatementGenerator = struct {
    pub fn generate(ir: *IRState, input_stmt: AST.Statement) IRError!void {
        switch (input_stmt) {
            .ReturnStatement => |stmt| {
                const given_type = try ExpressionGenerator.generate_rvalue(ir, stmt);
                const expected_type = ir.tm.functions.get(ir.scope.top().parent_func_name.?).?;
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
                ir.program_append(.push, var_info.stack_addr);
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
                } else if (t_info.tag == .Float) {
                    ir.program_append(.temp_print, 2);
                } else {
                    ir.program_append(.temp_print, 0);
                }
            },
        }
    }

    fn generate_conditional(ir: *IRState, block: AST.Statement) !void {
        const jump_back_ip = ir.program_len();

        const condition_info = switch (block) {
            .WhileStatement => |stmt| try ExpressionGenerator.generate_rvalue(ir, stmt.condition),
            .IfStatement => |stmt| try ExpressionGenerator.generate_rvalue(ir, stmt.condition),
            else => unreachable,
        };

        if (condition_info.tag != .Bool) {
            switch (block) {
                .IfStatement => |stmt| std.log.err("Cannot branch on non-boolean type {s}", .{stmt.start_loc}),
                .WhileStatement => |stmt| std.log.err("Cannot branch on non-boolean type {s}", .{stmt.start_loc}),
                else => unreachable,
            }
            return IRError.Syntax;
        }

        var block_scope = Block{
            .start_ip = ir.program_len(),
            .identifiers = std.StringHashMap(VarInfo).init(ir.allocator),
            .parent_func_name = ir.scope.buffer[0].parent_func_name,
        };
        ir.scope.push(&block_scope);
        defer ir.close_scope();

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

        ir.program.items[block_scope.start_ip + 1].operand = ir.program_len(); //plus 1 skips the not inst
    }
};

pub const ExpressionGenerator = struct {
    pub fn generate_rvalue(ir: *IRState, input_expr: AST.Expression) IRError!typing.TypeInfo {
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
                const idx = ir.function_ips.get(expr.name_tk.tag.Identifier) orelse {
                    std.log.err("Cannot find function {s}", .{expr.name_tk});
                    return IRError.Undeclared;
                };
                ir.program_append(.push, idx);
                ir.program_append(.call, ir.program_len() + 1);

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
                const info = try ir.find_identifier(token, ir.scope.top_idx());
                //hacky for sure maybe add a struct member for in ct_state
                if (info.ct_known and ir.active_buffer == &ir.program) {
                    ir.enter_ct_mode();
                    const type_info = try ExpressionGenerator.generate_rvalue(ir, input_expr);
                    const result = try ir.execute_ct_buffer();
                    ir.exit_ct_mode();
                    for (result) |op| {
                        ir.program_append(.push, op);
                    }
                    return type_info;
                }
                ir.program_append(.push, info.stack_addr);
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
                switch (expr.rhs) {
                    .RangeExpression => |r_expr| {
                        const input_info = try ExpressionGenerator.generate_lvalue(ir, expr.lhs);
                        if (input_info.tag != .Array and input_info.tag != .WidePointer) {
                            std.log.err("Cannot slice a type that is not an array or slice {s}", .{r_expr.op.loc});
                            return IRError.Syntax;
                        }
                        //correct order is length then ptr so comptime swap these boys
                        const lvalue_inst = ir.active_buffer.pop();
                        //a..b => b - a on the stack which is the length
                        _ = try ExpressionGenerator.generate_rvalue(ir, r_expr.rhs);
                        _ = try ExpressionGenerator.generate_rvalue(ir, r_expr.lhs);
                        ir.program_append(.add_i, 1);
                        //add the lower bound to the start of the array ptr
                        try ir.active_buffer.append(lvalue_inst);
                        _ = try ExpressionGenerator.generate_rvalue(ir, r_expr.lhs);
                        ir.program_append(.add_i, 0);
                        if (input_info.tag == .Array) {
                            return ir.tm.generate_wide_pointer(input_info.child.?.type_info.*);
                        } else {
                            @panic("not implemented");
                        }
                    },
                    else => {},
                }
                const info = try ExpressionGenerator.generate_lvalue(ir, input_expr);
                try ir.generate_mem_op(info, true, expr.op.loc);
                return info;
            },
            else => @panic("Not implemented"),
        }
    }
    fn generate_lvalue(ir: *IRState, input_expr: AST.Expression) !typing.TypeInfo {
        switch (input_expr) {
            .IdentifierInvokation => |token| {
                const var_info = try ir.find_identifier(token, ir.scope.top_idx());
                ir.program_append(.push, var_info.stack_addr);
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

    fn generate_access(ir: *IRState, expr: AST.Expression, input_info: typing.TypeInfo) !typing.TypeInfo {
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
                    .Record, .WidePointer => {
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
                if (input_info.tag != .Array and input_info.tag != .WidePointer) {
                    std.log.err("Tried to access a non indexible value {s}", .{expr});
                    return IRError.Syntax;
                }
                if (input_info.tag == .WidePointer) {
                    const data = input_info.child.?.field_info.get("data_ptr").?;
                    ir.program_append(.push, data.offset);
                    ir.program_append(.add_i, 0);
                    ir.program_append(.load8, null);
                    _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                    ir.program_append(.push, data.type_info.size);
                    ir.program_append(.mul_i, 0);
                    return data.type_info;
                } else {
                    _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                    ir.program_append(.push, input_info.child.?.type_info.size);
                    ir.program_append(.mul_i, 0);
                    return input_info.child.?.type_info.*;
                }
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
