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
//TODO: fix obscene amount of instructions generated for arrays
//TODO: decide on how much type checking to do in this step, perhaps all of it?
//TODO: assert array initialization matches specified length
//TODO: solution for general load store that can work on stack and arbitrary memory address

// x :: 33223;
// f
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
    functions: std.StringHashMap(usize), //function name to start_ip
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
                .ImportDeclaration => @panic("not implemented"),
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
            .functions = std.StringHashMap(usize).init(allocator),
            .stack_pointer = 0,
        };
    }

    pub fn generate_program(self: *Self) ![]Operation {
        defer self.deinit();
        errdefer self.program.deinit();
        self.program_append(.push, 0);
        self.program_append(.call, null);
        for (self.function_bodies) |f_decl| {
            var function_scope = Scope{
                .start_ip = self.program.items.len,
                .identifiers = std.StringHashMap(VarInfo).init(self.allocator),
            };
            self.stack_pointer = 8; //first 0 bytes for garbage
            self.scope_stack.push(&function_scope);
            defer function_scope.identifiers.deinit();
            defer self.scope_stack.pop();

            self.functions.put(f_decl.name_tk.tag.Identifier, function_scope.start_ip) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };

            var param_n = f_decl.params;
            while (param_n) |param| {
                const info = try self.register_var_decl(param.name_tk, param.typ);
                self.program_append(.push, info.stack_loc);
                try self.generate_stack_store(info.type_info, param.name_tk.loc);
                param_n = param.next;
            }

            for (f_decl.body) |stmt| {
                try StatementGenerator.generate(self, stmt);
            }

            if ((self.tm.functions.get(f_decl.name_tk.tag.Identifier).?).tag == .Void) {
                self.program_append(.ret, null);
            }
        }
        const idx = self.functions.get("main") orelse {
            std.log.err("no main function (main :: fn() {{...}})", .{});
            return IRError.Undeclared;
        };

        self.program.items[0].operand = idx;

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

    //NOTE: assume address is on the stack
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
                self.program_append(.tstore, null); //store the address into the gp reg
                const element_type = (type_info.child orelse @panic("compiler error")).type_info;
                const length = type_info.size / element_type.size;
                for (0..length) |i| {
                    const offset = @as(usize, @intCast(i)) * element_type.size;
                    self.program_append(.tload, null);
                    self.program_append(.push, @intCast(offset));
                    self.program_append(.add_i, 0);
                    try self.generate_stack_store(element_type.*, loc);
                }
            },
            .Record => {
                self.program_append(.tstore, null); //store the address into the gp reg
                for (type_info.child.?.field_info.values()) |field| {
                    self.program_append(.tload, null);
                    self.program_append(.push, field.offset);
                    self.program_append(.add_i, 0);
                    try self.generate_stack_store(field.type_info, loc);
                }
            },
            else => @panic("cannot yet store/load type"),
        }
    }

    fn generate_stack_load(self: *Self, type_info: typing.TypeInfo, loc: Location) !void {
        switch (type_info.tag) {
            .Void => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .Bool, .Byte, .Integer, .Float, .Pointer => {
                self.program_append(if (type_info.size == 1) .load else .load8, null);
            },
            .Array => {
                self.program_append(.tstore, null); //store the address into the gp reg
                const element_type = (type_info.child orelse @panic("compiler error")).type_info;
                const length = type_info.size / element_type.size;
                var i: isize = @intCast(length - 1);
                while (i >= 0) : (i -= 1) {
                    const offset = @as(usize, @intCast(i)) * element_type.size;
                    self.program_append(.tload, null);
                    self.program_append(.push, offset);
                    self.program_append(.add_i, 0);
                    try self.generate_stack_load(element_type.*, loc);
                }
            },
            .Record => {
                self.program_append(.tstore, null); //store the address into the gp reg
                var infos = type_info.child.?.field_info.values();
                std.mem.reverse(typing.FieldInfoStruct, infos);
                for (infos) |field| {
                    self.program_append(.tload, null);
                    self.program_append(.push, field.offset);
                    self.program_append(.add_i, 0);
                    try self.generate_stack_load(field.type_info, loc);
                }
                std.mem.reverse(typing.FieldInfoStruct, infos);
            },
            else => @panic("cannot yet store/load type"),
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
        self.functions.deinit();
    }
};

const StatementGenerator = struct {
    fn generate(ir: *IRGenerator, input_stmt: AST.Statement) !void {
        switch (input_stmt) {
            .ReturnStatement => |stmt| {
                _ = try ExpressionGenerator.generate_rvalue(ir, stmt);
                ir.program_append(.ret, null);
            },
            .ExpressionStatement => |expr| {
                _ = try ExpressionGenerator.generate_rvalue(ir, expr);
            },
            .VariableDeclaration => |stmt| {
                if (stmt.typ == null) @panic("variable type inference not implemented");
                const var_info = try ir.register_var_decl(stmt.name_tk, stmt.typ.?);
                const assignment = stmt.assignment;
                _ = try ExpressionGenerator.generate_rvalue(ir, assignment);
                ir.program_append(.push, var_info.stack_loc);
                try ir.generate_stack_store(var_info.type_info, stmt.name_tk.loc);
            },
            .VariableAssignment => |stmt| {
                _ = try ExpressionGenerator.generate_rvalue(ir, stmt.rhs);
                const type_info = try ExpressionGenerator.generate_lvalue(ir, stmt.lhs);
                try ir.generate_stack_store(type_info, stmt.loc);
            },
            .IfStatement => |stmt| {
                const info = try ExpressionGenerator.generate_rvalue(ir, stmt.condition);
                if (info.tag != .Bool) {
                    std.log.info("Cannot branch on non-boolean type {s}", .{stmt.start_loc});
                    return IRError.Syntax;
                }

                var if_scope = IRGenerator.Scope{
                    .start_ip = ir.program.items.len,
                    .identifiers = std.StringHashMap(VarInfo).init(ir.allocator),
                };
                ir.scope_stack.push(&if_scope);
                defer if_scope.identifiers.deinit();
                defer ir.scope_stack.pop();

                ir.program_append(.not, null);
                ir.program_append(.je, 1);
                for (stmt.body) |body_stmt| {
                    try StatementGenerator.generate(ir, body_stmt);
                }
                //plus 1 skips the not
                ir.program.items[if_scope.start_ip + 1].operand = ir.program.items.len;
            },
            .WhileStatement => |stmt| {
                const jump_back_ip = ir.program.items.len;
                const info = try ExpressionGenerator.generate_rvalue(ir, stmt.condition);
                if (info.tag != .Bool) {
                    std.log.info("Cannot branch on non-boolean type {s}", .{stmt.start_loc});
                    return IRError.Syntax;
                }

                var while_scope = IRGenerator.Scope{
                    .start_ip = ir.program.items.len,
                    .identifiers = std.StringHashMap(VarInfo).init(ir.allocator),
                };
                ir.scope_stack.push(&while_scope);
                defer while_scope.identifiers.deinit();
                defer ir.scope_stack.pop();
                ir.program_append(.not, null);
                ir.program_append(.je, 1);
                for (stmt.body) |body_stmt| {
                    try StatementGenerator.generate(ir, body_stmt);
                }
                ir.program_append(.jmp, jump_back_ip);
                ir.program.items[while_scope.start_ip + 1].operand = ir.program.items.len;
            },
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
};

const ExpressionGenerator = struct {
    fn generate_rvalue(ir: *IRGenerator, input_expr: AST.Expression) IRError!typing.TypeInfo {
        switch (input_expr) {
            .LiteralInt => |token| {
                ir.program_append(.push, @bitCast(token.tag.Integer));
                return typing.Primitive.get("int").?;
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
                //push args onto stack
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
                        try ir.generate_stack_load(ti_info.child.?.type_info.*, expr.op.loc);
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
                try ir.generate_stack_load(info.type_info, token.loc);
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
            .AccessExpression => |expr| {
                const info = try ExpressionGenerator.generate_lvalue(ir, input_expr);
                try ir.generate_stack_load(info, expr.op.loc);
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
                const type_info = try ExpressionGenerator.generate_access_r(ir, expr.rhs, initial_info);
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
            else => @panic("not implemented"),
        }
    }

    fn generate_access_r(ir: *IRGenerator, expr: AST.Expression, input_info: typing.TypeInfo) !typing.TypeInfo {
        switch (expr) {
            .IdentifierInvokation => |field_tk| {
                if (input_info.tag != .Record) {
                    if (input_info.tag != .Array) {
                        std.log.err("Cannot access non array or record {s}", .{field_tk});
                        return IRError.Syntax;
                    }
                    _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                    ir.program_append(.push, input_info.child.?.type_info.size);
                    ir.program_append(.mul_i, 0);
                    return input_info.child.?.type_info.*;
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
                        ir.program_append(.add_i, 0);
                        return rhs_info;
                    },
                    .Array => {
                        _ = try generate_access_r(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access_r(ir, a_expr.rhs, input_info.child.?.type_info.*);
                        ir.program_append(.add_i, 0);
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
