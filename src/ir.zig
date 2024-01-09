const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");
const typing = @import("typing.zig");
const Stack = @import("common.zig").Stack;

const Operation = struct {
    const Opcode = enum(u8) {
        load_8,
        load_64,
        store_8,
        store_64,
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
        for (self.declarations) |decl| {
            switch (decl) {
                .RecordDeclaration => |r_decl| {
                    try self.tm.register_record(r_decl);
                },
                .FunctionDeclaration => |f_decl| {
                    try self.tm.register_function(f_decl);

                    //open the function scope
                    var function_scope = Scope.init(self.allocator);
                    self.stack_pointer = 0;
                    self.scopes.push(&function_scope);
                    defer self.scopes.pop();
                    defer function_scope.deinit();

                    //NOTE: body begins here may be needed in the future

                    var param_n = f_decl.params;
                    while (param_n) |param| {
                        const type_info = try self.register_var_decl(param.name_tk, param.typ);
                        //call a store here becuase param value should be on operand stack from func call expression
                        self.program_append(if (type_info.size == 8) .store_8 else .store_64, null);
                        param_n = param.next;
                    }
                },
                .ConstantDeclaration => |c_decl| {
                    _ = c_decl;
                    @panic("constant declarations not currently compilable");
                },
            }
        }
    }

    //does not generate any instructions just adds it to the scope
    fn register_var_decl(self: *Self, name_tk: Token, dt: AST.DefinedType) !typing.TypeInfo {
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
        self.stack_pointer += typ.size;

        return typ;
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
