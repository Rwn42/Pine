const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");
const typing = @import("typing.zig");
const Stack = @import("common.zig").Stack;

//TODO: Record initialization
//TODO: array access
//TODO: record access (nested included) no deref yet

const Operation = struct {
    const Opcode = enum(u8) {
        load_8, //NOTE: operand is the stack location to load from
        load_64, //NOTE: operand is the stack location to load from
        store_8, //NOTE: operand is the stack location to store to
        store_64, //NOTE: operand is the stack location to store to
        push, //NOTE: operand is 8 byte number which is the data to push
        add_i,
        add_f,
        mul_i,
        mul_f,
        lt,
        lte,
        eq,
        not,
        jmp,
        je,
        call,
        ret,
    };

    opc: Opcode,
    operand: ?u64, // the signedness of the integer does not matter just 8 bytes

    pub fn format(self: Operation, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} ", .{@tagName(self.opc)});
        if (self.operand) |operand| try writer.print("{d}", .{operand});
    }
};

pub const IRError = error{
    DuplicateDefinition,
    Undeclared,
    TypeMismatch,
};

pub const IRGenerator = struct {
    const VarInfo = struct {
        type_info: typing.TypeInfo,
        stack_loc: u64,
    };
    const Scope = std.StringHashMap(VarInfo);
    const Self = @This();

    stack_pointer: usize, //stack memory pointer will reset to 0 for each different func call
    scopes: Stack(*Scope, 32), //global scope + function scope + 30 nested blocks seems good enough
    declarations: []AST.Declaration,
    allocator: std.mem.Allocator,
    program: std.ArrayList(Operation),
    tm: typing.TypeManager,

    pub fn init(declarations: []AST.Declaration, allocator: std.mem.Allocator) Self {
        return .{
            .stack_pointer = 0,
            .scopes = Stack(*Scope, 32).init(),
            .declarations = declarations,
            .allocator = allocator,
            .program = std.ArrayList(Operation).init(allocator),
            .tm = typing.TypeManager.init(allocator),
        };
    }

    pub fn generate(self: *Self) !void {
        //first loop skips all the bodies and deals with the declaration
        //basically creating a "header file" in memory
        for (self.declarations) |decl| {
            switch (decl) {
                .RecordDeclaration => |r_decl| {
                    if (self.tm.records.contains(r_decl.name_tk.tag.Identifier)) {
                        std.log.err("Duplicate record definition {s}", .{r_decl.name_tk});
                        return IRError.DuplicateDefinition;
                    }
                    self.tm.records.put(r_decl.name_tk.tag.Identifier, undefined) catch {
                        @panic("FATAL COMPILER ERROR: Out of memory!");
                    };
                },
                .FunctionDeclaration => |f_decl| {
                    try self.tm.register_function(f_decl);
                },
                else => {},
            }
        }

        //deal with the bodies
        for (self.declarations) |decl| {
            switch (decl) {
                .RecordDeclaration => |r_decl| {
                    try self.tm.register_record(r_decl);
                },
                .FunctionDeclaration => |f_decl| {
                    //open the function scope
                    var function_scope = Scope.init(self.allocator);
                    self.stack_pointer = 0;
                    self.scopes.push(&function_scope);
                    defer self.scopes.pop();
                    defer function_scope.deinit();

                    //NOTE: body begins here may be needed in the future

                    var param_n = f_decl.params;
                    while (param_n) |param| {
                        const info = try self.register_var_decl(param.name_tk, param.typ);
                        try self.generate_stack_store(info);
                        param_n = param.next;
                    }

                    for (f_decl.body) |stmt| {
                        try self.generate_statement(stmt);
                    }
                },
                .ConstantDeclaration => |c_decl| {
                    _ = c_decl;
                    @panic("constant declarations not currently compilable");
                },
            }
        }
    }

    fn generate_statement(self: *Self, input_stmt: AST.Statement) !void {
        switch (input_stmt) {
            .ReturnStatement => |stmt| {
                try self.generate_expression(stmt);
                self.program_append(.ret, null);
            },
            .VariableDeclaration => |stmt| {
                if (stmt.typ == null) @panic("type inference (:=) not implemented");
                const info = try self.register_var_decl(stmt.name_tk, stmt.typ.?);
                if (stmt.assignment) |assignment| {
                    try self.generate_expression(assignment);
                    try self.generate_stack_store(info);
                }
            },
            else => @panic("not implemented"),
        }
    }

    fn generate_expression(self: *Self, input_expr: AST.Expression) !void {
        switch (input_expr) {
            .IdentifierInvokation => |id_tk| {
                const id_info = try self.find_identifier(id_tk, self.scopes.top_idx());
                if (id_info.type_info.size != 8 and id_info.type_info.size != 64) {
                    std.log.err("Cannot use identifier {s} of size {d} bytes in expression", .{
                        id_tk,
                        id_info.type_info.size / 8,
                    });
                }
                self.program_append(if (id_info.type_info.size == 8) .load_8 else .load_64, id_info.stack_loc);
            },
            .ArrayInitialization => |list| {
                var node: ?*AST.ExprList = list;
                while (node) |node_val| {
                    try self.generate_expression(node_val.expr);
                    node = node_val.next;
                }
            },
            .RecordInitialization => |r| {
                const info = self.tm.records.get(r.name_tk.tag.Identifier) orelse {
                    std.log.err("Undeclared record type {s}", .{r.name_tk});
                    return IRError.Undeclared;
                };

                //incredibly naive solution ast should store fields in a map
                //linked list is awful for this
                //for each field in the record it iterates the whole
                //record initialization node
                outer: for (info.child.?.field_info.keys()) |key| {
                    var node: ?*AST.FieldList = r.fields;
                    while (node) |node_val| {
                        if (std.mem.eql(u8, key, node_val.field.tag.Identifier)) {
                            try self.generate_expression(node_val.expr);
                            continue :outer;
                        }
                        node = node_val.next;
                    }
                }
            },
            else => @panic("not implemented"),
        }
    }

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.scopes.get(idx).get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("No variable declared for identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    //does not generate any instructions just adds it to the scope
    fn register_var_decl(self: *Self, name_tk: Token, dt: AST.DefinedType) !VarInfo {
        const typ = try self.tm.generate(dt);
        if (self.scopes.top().contains(name_tk.tag.Identifier)) {
            std.log.err("Duplicate definition of identifier {s}", .{name_tk});
            return IRError.DuplicateDefinition;
        }
        self.scopes.top().put(name_tk.tag.Identifier, .{
            .type_info = typ,
            .stack_loc = self.stack_pointer,
        }) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
        self.stack_pointer += typ.size / 8;

        return self.scopes.top().get(name_tk.tag.Identifier).?;
    }

    fn generate_stack_store(self: *Self, info: VarInfo) !void {
        const type_info = info.type_info;
        if (type_info.tag != .Record and type_info.tag != .Array) {
            self.program_append(if (type_info.size == 8) .store_8 else .store_64, info.stack_loc);
            return;
        }
        if (type_info.tag == .Array) {
            const element_type = type_info.child.?.type_info;
            const length = type_info.size / element_type.size;

            for (0..length) |i| {
                self.program_append(if (element_type.size == 8) .store_8 else .store_64, info.stack_loc + i * (element_type.size / 8));
            }

            return;
        }

        //record store
        var it = info.type_info.child.?.field_info.iterator();
        while (it.next()) |entry| {
            const field = entry.value_ptr;
            if (field.type_info.size != 8 and field.type_info.size != 64) {
                try self.generate_stack_store(.{ .stack_loc = info.stack_loc + field.offset, .type_info = field.type_info });
            } else {
                self.program_append(if (field.type_info.size == 8) .store_8 else .store_64, info.stack_loc + field.offset);
            }
        }
    }

    fn program_append(self: *Self, opc: Operation.Opcode, ope: ?u64) void {
        self.program.append(.{ .opc = opc, .operand = ope }) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    pub fn deinit(self: *Self) void {
        self.tm.deinit();
    }
};

//  pub fn generate(self: *Self) !void {
//         for (self.declarations) |decl_type| {
//             switch (decl_type) {
//                 .FunctionDeclaration => |fndecl| {
//                     //TODO: register its type
//                     self.sp = 0;
//                     var function_scope = ScopeT.init(self.allocator);
//                     self.scopes.push(&function_scope);
//                     errdefer function_scope.deinit();

//                     for (fndecl.body) |stmt| {
//                         try self.compile_stmt(stmt);
//                     }

//                     self.scopes.pop();
//                     function_scope.deinit();
//                 },
//                 else => {
//                     @panic("Not Implemented");
//                 },
//             }
//         }
//     }

//     fn compile_stmt(self: *Self, stmt: AST.Statement) !void {
//         switch (stmt) {
//             .ExpressionStatement => |expr| try self.compile_expression(expr),
//             .ReturnStatement => |expr| {
//                 try self.compile_expression(expr);
//                 self.program_append(.ret, 419);
//             },
//             .VariableDeclaration => |decl| {
//                 const name = decl.name_tk.tag.Identifier;
//                 if (self.scopes.top().contains(name)) {
//                     std.log.err("Duplicate defintion of variable {s}", .{decl.name_tk});
//                     return IRError.DuplicateDefinition;
//                 }
//                 const typ = try typing.TypeInfo.gen(decl.typ.?); //TODO: type inference
//                 self.scopes.top().put(name, .{ .type_info = typ, .stack_loc = self.sp }) catch {
//                     @panic("FATAL COMPILER ERROR: Out of memory");
//                 };
//                 self.sp += 1;

//                 if (decl.assignment) |assignment| {
//                     try self.compile_expression(assignment); //assignment expression
//                     self.program_append(.push, self.sp - 1); //addr of var
//                     self.program_append(.s_store, typ.size); //store the var
//                 }
//             },
//             .VariableAssignment => |assignment| {
//                 const name = assignment.name_tk.tag.Identifier;
//                 const info = self.find_info(name, self.scopes.sp - 1) catch |err| {
//                     std.log.err("Undeclared Identifier {s}", .{assignment.name_tk});
//                     return err;
//                 };
//                 try self.compile_expression(assignment.assignment);
//                 self.program_append(.push, info.stack_loc);
//                 self.program_append(.s_store, info.type_info.size);
//             },
//             else => @panic("not implemented"),
//         }
//     }

//     //traverse through open scopes to find identifier
//     fn find_info(self: *Self, id: []u8, idx: usize) !IdInfo {
//         if (self.scopes.get(idx).get(id)) |info| return info;
//         if (idx == 0) return IRError.Undeclared;
//         return try self.find_info(id, idx - 1);
//     }

//     fn compile_expression(self: *Self, expr: AST.Expression) !void {
//         switch (expr) {
//             .LiteralInt => |tk| self.program_append(.push, @bitCast(tk.tag.Integer)),
//             .LiteralBool => |tk| switch (tk.tag) {
//                 .True => self.program_append(.push, 1),
//                 .False => self.program_append(.push, 0),
//                 else => unreachable,
//             },
//             .LiteralFloat => |tk| self.program_append(.push, @bitCast(tk.tag.Float)),
//             .LiteralString => @panic("Not Implemented"),
//             .IdentifierInvokation => |tk| {
//                 const name = tk.tag.Identifier;
//                 const info = self.find_info(name, self.scopes.sp - 1) catch |err| {
//                     std.log.err("Undeclared Identifier {s}", .{tk});
//                     return err;
//                 };
//                 self.program_append(.push, info.stack_loc);
//                 self.program_append(.s_load, info.type_info.size);
//             },
//             .BinaryExpression => |expr1| {
//                 const typ = try self.infer_type(expr);
//                 if (typ.tag == .Bool) {
//                     switch (expr1.op.tag) {
//                         .GreaterThanEqual => {
//                             try self.compile_expression(expr1.rhs);
//                             try self.compile_expression(expr1.lhs);
//                             self.program_append(.lt, 0);
//                         },
//                         .GreaterThan => {
//                             try self.compile_expression(expr1.rhs);
//                             try self.compile_expression(expr1.lhs);
//                             self.program_append(.lt, 1);
//                         },
//                         .LessThan => {
//                             try self.compile_expression(expr1.lhs);
//                             try self.compile_expression(expr1.rhs);
//                             self.program_append(.lt, 0);
//                         },
//                         .LessThanEqual => {
//                             try self.compile_expression(expr1.lhs);
//                             try self.compile_expression(expr1.rhs);
//                             self.program_append(.lt, 1);
//                         },
//                         .DoubleEqual => {
//                             try self.compile_expression(expr1.lhs);
//                             try self.compile_expression(expr1.rhs);
//                             self.program_append(.eq, 0);
//                         },
//                         .NotEqual => {
//                             try self.compile_expression(expr1.lhs);
//                             try self.compile_expression(expr1.rhs);
//                             self.program_append(.eq, 1);
//                         },
//                         else => unreachable,
//                     }
//                     return;
//                 }
//                 if (typ.tag != .Integer and typ.tag != .Float) {
//                     std.log.err("cannot perform operation on types other than integers and floats at {s}", .{expr1.op});
//                     return IRError.TypeMismatch;
//                 }
//                 switch (expr1.op.tag) {
//                     .Plus => self.program_append(if (typ.tag == .Integer) .i_add else .f_add, 1),
//                     .Dash => self.program_append(if (typ.tag == .Integer) .i_add else .f_add, 0),
//                     .Asterisk => self.program_append(if (typ.tag == .Integer) .i_mul else .f_mul, 1),
//                     .SlashForward => self.program_append(if (typ.tag == .Integer) .i_mul else .f_mul, 0),
//                     else => unreachable,
//                 }
//             },
//             else => @panic("Not implemented"),
//         }
//     }

//     fn infer_type(self: *Self, expr: AST.Expression) !typing.TypeInfo {
//         return switch (expr) {
//             .LiteralInt => typing.Primitive.get("int").?,
//             .LiteralFloat => typing.Primitive.get("float").?,
//             .LiteralBool => typing.Primitive.get("bool").?,
//             .LiteralString => @panic("not implemented"),
//             .IdentifierInvokation => |invokation| blk: {
//                 const info = self.find_info(invokation.tag.Identifier, self.scopes.sp - 1) catch |err| {
//                     std.log.err("Undeclared Identifier {s}", .{invokation.tag});
//                     return err;
//                 };
//                 break :blk info.type_info;
//             },
//             .BinaryExpression => |expr1| blk: {
//                 if (expr1.op.tag == .Dot) @panic("not implemented"); //record access
//                 const operator_controls: ?typing.TypeInfo = switch (expr1.op.tag) {
//                     .Dot => @panic("not implemented"),
//                     .GreaterThan,
//                     .LessThan,
//                     .LessThanEqual,
//                     .NotEqual,
//                     .DoubleEqual,
//                     .GreaterThanEqual,
//                     => typing.Primitive.get("bool"),
//                     else => null,
//                 };
//                 if (operator_controls) |typ| return typ;
//                 const tlhs = try self.infer_type(expr1.lhs);
//                 const trhs = try self.infer_type(expr1.rhs);
//                 if (tlhs.tag != trhs.tag) {
//                     std.log.err("Type mismtach: left hand side is type {s} right hand side is type {s} here {s}", .{ @tagName(tlhs.tag), @tagName(trhs.tag), expr1.op });
//                     return IRError.TypeMismatch;
//                 }
//                 break :blk tlhs;
//             },
//             .UnaryExpression => |expr1| blk: {
//                 break :blk switch (expr1.op.tag) {
//                     .ExclamationMark => typing.Primitive.get("bool").?,
//                     .Hat => @panic("Not implemented"),
//                     .Ampersand => .{ .size = 8, .tag = .Pointer },
//                     else => unreachable,
//                 };
//             },
//             else => @panic("not implemented"),
//         };
//     }

//     fn program_append(self: *Self, opc: Opcode, ope: u64) void {
//         self.program.append(.{ .opc = opc, .operand = ope }) catch {
//             @panic("FATAL COMPILER ERROR: Out of memory");
//         };
//     }

//     pub fn deinit(self: *Self) void {
//         _ = self; // autofix
//     }

// ----IR Plan probably not smart----
//each stack needs a map of strings to integers where the integer is where in the stack memory the variable lies
// stack_mem: []u8 -> {0, 0 ,0 ...}
// a: byte = 10 -> {10, 0, 0} map location is 0
//some_struct: SomeStruct = {x: 11, y: 12}; -> {10, 11, 12} some_struct at 1
//some_struct.x 1 + offset of x
