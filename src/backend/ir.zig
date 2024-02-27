const std = @import("std");

const typing = @import("typing.zig");
const lexing = @import("../frontend/lexer.zig");
const parsing = @import("../frontend/parser.zig");
const ast = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;
const FileLocation = @import("../frontend/token.zig").FileLocation;

pub const IRError = error{
    Undeclared,
    OutOfMemory,
    Duplicate,
    Syntax,
};

const CCall = struct {
    name: []const u8,
    info: typing.FuncInfo,
};

const Call = struct {
    name: []const u8,
    param_size: usize,
};

pub const IRInstruction = union(enum) {
    Function: []const u8,
    PushB: u8,
    PushW: usize,
    Call: Call,
    CCall: CCall,
    Reserve: usize, //moves the stack pointer but does not push a value useful for reserving > word size values
    StoreW,
    StoreB,
    LoadW,
    LoadB,
    Ret,
    StackAddr: usize, //pointer to the start of the variable section of the stack
    ParamAddr: usize, //pointer to start of param section (could be accomplshed with var addr but its clunky)
    StaticAddr: usize, //pointer to start of static memory
    Add_I: bool,
    Add_F: bool,
    Mul_I: bool,
    Mul_F: bool,
    LT_I,
    LT_F,
    GT_I,
    GT_F,
    Jmp: []u8, //label name could be done without labels by knowing the scale factor of IR->target instructions
    Jz: []u8, //label name
    Label: []u8,
    Eq,
    Not,
};

pub fn generate_file_ir(types: typing.FileTypes, functions: []*ast.FunctionDeclarationNode, allocator: std.mem.Allocator) IRError!FileIR {
    var ir: FileIR = undefined;

    //all imported functions need to be labeleld with extern also with pine name mangling (pine_{name})
    var imported_buffer = std.ArrayList([]const u8).init(allocator);
    defer imported_buffer.deinit();
    for (types.imported_types) |imported| {
        for (imported.function_types.values(), imported.function_types.keys()) |func_type, name| {
            if (func_type.public) {
                try imported_buffer.append(name);
            }
        }
    }
    ir.imported = try imported_buffer.toOwnedSlice();
    errdefer allocator.free(ir.imported);

    //pupblic functions need to be marked as such for the linker to see
    //external functions need to be seperate from imported as they do not have name mangling
    var public_buffer = std.ArrayList([]const u8).init(allocator);
    var extern_buffer = std.ArrayList([]const u8).init(allocator);
    defer public_buffer.deinit();
    var func_type_iter = types.function_types.iterator();
    while (func_type_iter.next()) |f_type| {
        if (f_type.value_ptr.public) {
            try public_buffer.append(f_type.key_ptr.*);
        } else if (f_type.value_ptr.external) {
            //std.log.info("external {s}", .{f_type.key_ptr.*});
            try extern_buffer.append(f_type.key_ptr.*);
        }
    }

    //these have to panic or else and errdefer stmt would be needed after the first owned slice to avoid a leak
    ir.external = extern_buffer.toOwnedSlice() catch @panic("Out of memory!");
    ir.public = public_buffer.toOwnedSlice() catch @panic("Out of memory!");
    errdefer allocator.free(ir.external);
    errdefer allocator.free(ir.public);

    //here we set up the state to actual begin generating IR
    var execution_state = ExecutionState.init(allocator, &types);
    defer execution_state.deinit();

    var static_scope = ExecutionState.Block.init(execution_state.allocator);
    execution_state.scopes.push(&static_scope);
    defer static_scope.deinit();
    defer execution_state.scopes.pop();

    for (functions) |f_decl| {
        try execution_state.generate_function(f_decl);
    }

    //at this point all executable code has been generated and we can add it to the IR
    //these have to panic or else and errdefer stmt would be needed after the first owned slice to avoid a leak
    ir.instructions = execution_state.program.toOwnedSlice() catch @panic("Out of memory!");
    ir.static = execution_state.static.toOwnedSlice() catch @panic("Out of memory!");
    ir.label_arena = execution_state.label_arena;

    scratch_interpret(ir);
    return ir;
}

pub const FileIR = struct {
    public: [][]const u8,
    external: [][]const u8,
    imported: [][]const u8,
    static: []u8,
    instructions: []IRInstruction,
    label_arena: std.heap.ArenaAllocator,

    pub fn deinit(ir: FileIR, allocator: std.mem.Allocator) void {
        allocator.free(ir.public);
        allocator.free(ir.external);
        allocator.free(ir.static);
        allocator.free(ir.instructions);
        allocator.free(ir.imported);
        ir.label_arena.deinit();
    }
};

const ExecutionState = struct {
    const Self = @This();

    const Block = std.StringHashMap(VarInfo);
    const Scope = Stack(*Block, 32, "Too many Nested Blocks");

    const MemoryLocation = enum {
        Stack,
        Parameter, //technically also on the stack just the callers stack not the callees stack
        Static,
    };

    const VarInfo = struct {
        offset: usize,
        type_info: typing.TypeInfo,
        location: MemoryLocation,
    };

    program: std.ArrayList(IRInstruction),
    static: std.ArrayList(u8),
    allocator: std.mem.Allocator,
    scopes: Scope = Scope.init(),
    types: *const typing.FileTypes,

    current_function_name: []const u8 = undefined,

    label_id: usize = 0, //increments per label being created besides functions
    label_arena: std.heap.ArenaAllocator, //eventually we will just jump to instructions addresses

    stack_addr: usize = 0,

    fn init(allocator: std.mem.Allocator, types: *const typing.FileTypes) Self {
        return .{
            .program = std.ArrayList(IRInstruction).init(allocator),
            .static = std.ArrayList(u8).init(allocator),
            .allocator = allocator,
            .types = types,
            .label_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        self.static.deinit();
        self.program.deinit();
    }

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.scopes.get(idx).get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("Undeclared identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    fn push_variable_address(self: *Self, info: VarInfo) !void {
        switch (info.location) {
            .Static => try self.program.append(.{ .StaticAddr = info.offset }),
            .Stack => try self.program.append(.{ .StackAddr = info.offset }),
            .Parameter => try self.program.append(.{ .ParamAddr = info.offset }),
        }
    }

    fn program_len(self: *Self) usize {
        return self.program.items.len;
    }

    //NOTE: register must be done before the expression/assignment has been gene
    fn register_var_decl(self: *Self, name_tk: Token, typ: typing.TypeInfo, comptime param: bool) !VarInfo {
        //so far this language allows shadowing thus only conflicts if defined in same scope
        if (self.scopes.top().contains(name_tk.tag.Identifier)) {
            std.log.err("Duplicate definition of identifier {s}", .{name_tk});
            return IRError.Duplicate;
        }

        //scope index 1 (global scope) contains static memory (writeable but static)
        const is_static = self.scopes.top_idx() == 0;

        //any other scope is a variable or parameter
        try self.scopes.top().put(name_tk.tag.Identifier, .{
            .type_info = typ,
            .offset = if (is_static) self.static.items.len else self.stack_addr,
            .location = if (is_static) .Static else if (param) .Parameter else .Stack,
        });

        //I dont incremenent a static pointer if static because some items implicitly are static so I handle it elsewhere
        if (!is_static) self.stack_addr += typ.size;

        return self.scopes.top().get(name_tk.tag.Identifier).?;
    }

    //assumes the initial address is on the stack
    //generates nessecary instructions to load/store the value into the address on the stack
    fn generate_mem_op(self: *Self, type_info: typing.TypeInfo, comptime load: bool, loc: FileLocation) !void {
        switch (type_info.tag) {
            .PineVoid => {
                std.log.err("Cannot have variable/field of type void {s}", .{loc});
                return IRError.Syntax;
            },
            .PineBool, .PineByte, .PineInt, .PineFloat, .PinePtr, .PineUntypedInt, .PineWord => {
                if (load) {
                    try self.program.append(if (type_info.size == 1) .LoadB else .LoadW);
                } else {
                    try self.program.append(if (type_info.size == 1) .StoreB else .StoreW);
                }
            },
            .PineRecord => |fields| {
                //store offset for future reference
                try self.program.append(.{ .StackAddr = 0 });
                try self.program.append(.StoreW);
                const record_vals = fields.values();

                //if loading from memory load onto stack in reverse order so it could be stored again
                //TODO: check if this reverse persists across calls concerning the same record
                if (load) std.mem.reverse(typing.FieldInfo, record_vals);
                for (record_vals) |field| {
                    try self.program.append(.{ .StackAddr = 0 });
                    try self.program.append(.LoadW);
                    try self.program.append(.{ .PushW = field.offset });
                    try self.program.append(.{ .Add_I = false });
                    try self.generate_mem_op(field.type_info, load, loc);
                }
            },
            .PineWidePointer => {
                try self.program.append(.{ .StackAddr = 0 });
                try self.program.append(.StoreW);
                try self.program.append(.{ .StackAddr = 0 });
                try self.program.append(.LoadW);
                try self.generate_mem_op(typing.PinePrimitive.get("usize").?, load, loc);
                try self.program.append(.{ .StackAddr = 0 });
                try self.program.append(.LoadW);
                try self.program.append(.{ .PushW = 8 });
                try self.program.append(.{ .Add_I = false });
                try self.generate_mem_op(.{ .child = null, .tag = .PinePtr, .size = 8 }, load, loc);
            },
            .PineArray => {
                const child_info = try self.types.from_ast(type_info.child.?);
                const length = type_info.size / child_info.size;
                try self.program.append(.{ .StackAddr = 0 });
                try self.program.append(.StoreW);
                for (0..length) |i| {
                    try self.program.append(.{ .StackAddr = 0 });
                    try self.program.append(.LoadW);
                    try self.program.append(.{ .PushW = i * child_info.size });
                    try self.program.append(.{ .Add_I = false });
                    try self.generate_mem_op(child_info, load, loc);
                }
            },
        }
    }

    fn new_label(self: *Self) ![]u8 {
        const label_name = try std.fmt.allocPrint(self.label_arena.allocator(), "PineLabel_{d}", .{self.label_id});
        self.label_id += 1;
        return label_name;
    }

    fn generate_function(self: *Self, func: *ast.FunctionDeclarationNode) !void {
        self.current_function_name = func.name_tk.tag.Identifier;
        self.label_id = 0;

        var scope = Block.init(self.allocator);
        self.scopes.push(&scope);
        defer scope.deinit();
        defer self.scopes.pop();

        const func_type = self.types.function_types.get(func.name_tk.tag.Identifier).?;
        try self.program.append(.{ .Function = func.name_tk.tag.Identifier });
        const start_idx = self.program_len();
        try self.program.append(.{ .Reserve = undefined }); //we dont know size yet so leave undefined

        //since params are "negative address" for us we have to start "above" the first one
        if (func_type.params.len >= 1) self.stack_addr += func_type.params[0].size;

        //clunky but param addresses grow downwards because first param is pushed last
        //alternatvie is to copy every parameter from the old function frame to the new one
        //that idea is better from a stack based vm approach but not for codegen atleast as far as i can tell
        var type_iter = std.mem.reverseIterator(func_type.params);
        var node_iter = std.mem.reverseIterator(func.params);

        while (node_iter.next()) |ast_p| {
            const p_info = type_iter.next().?;
            _ = try self.register_var_decl(ast_p.name_tk, p_info, true);
        }
        //NOTE: first 8 bytes reserved for temporary information like struct offset ect
        self.stack_addr = 8; //params are done so reset stack_addr since param stack addressed are "negative"

        for (func.body) |stmt| {
            try StatementGenerator.generate(self, stmt);
        }
        try self.program.append(.Ret); //does not help if function returns a value but the user did not

        self.program.items[start_idx] = .{ .Reserve = self.stack_addr };
        self.stack_addr = 0;
    }
};

const StatementGenerator = struct {
    fn generate(s: *ExecutionState, input_stmt: ast.Statement) IRError!void {
        try switch (input_stmt) {
            .ReturnStatement => |expr| {
                if (expr) |real_expr| {
                    const function_type = s.types.function_types.get(s.current_function_name).?;
                    const expr_type = try ExpressionGenerator.generate_rvalue(s, real_expr);
                    try typing.equivalent(expr_type, function_type.return_type);
                    try s.program.append(.{ .ParamAddr = function_type.return_type.size + function_type.param_size });
                    try s.generate_mem_op(expr_type, false, real_expr.location());
                }
                try s.program.append(.Ret);
            },
            .ExpressionStatement => |expr| {
                const info = try ExpressionGenerator.generate_rvalue(s, expr);
                try typing.equivalent(info, typing.PinePrimitive.get("void").?);
            },
            //NOTE: this statement cannot be used for static declarations as rvalues come first
            .VariableDeclaration => |stmt| {
                var given_info = try ExpressionGenerator.generate_rvalue(s, stmt.assignment);

                if (stmt.typ) |typ| {
                    const declared_info = try s.types.from_ast(typ);
                    try typing.equivalent(given_info, declared_info);
                    given_info = declared_info;
                }
                const info = try s.register_var_decl(stmt.name_tk, given_info, false);
                try s.push_variable_address(info);
                try s.generate_mem_op(given_info, false, stmt.name_tk.location);
            },
            .VariableAssignment => |stmt| {
                const rhs_info = try ExpressionGenerator.generate_rvalue(s, stmt.rhs);
                const lhs_info = try ExpressionGenerator.generate_lvalue(s, stmt.lhs);
                try typing.equivalent(rhs_info, lhs_info);
                try s.generate_mem_op(lhs_info, false, stmt.loc);
            },
            .IfStatement => generate_conditional(s, input_stmt, false),
            .WhileStatement => generate_conditional(s, input_stmt, true),
        };
    }

    fn generate_conditional(s: *ExecutionState, input_stmt: ast.Statement, comptime is_while: bool) !void {
        const label_name_start = try s.new_label();
        const label_name_end = try s.new_label();

        var stmt = if (is_while) input_stmt.WhileStatement else input_stmt.IfStatement;

        if (is_while) try s.program.append(.{ .Label = label_name_start });

        const condition = try ExpressionGenerator.generate_rvalue(s, stmt.condition);
        try typing.equivalent(condition, typing.PinePrimitive.get("bool").?);

        var scope = ExecutionState.Block.init(s.allocator);
        s.scopes.push(&scope);
        defer scope.deinit();
        defer s.scopes.pop();

        try s.program.append(.{ .Jz = label_name_end });
        for (stmt.body) |body_stmt| {
            try StatementGenerator.generate(s, body_stmt);
        }
        if (is_while) try s.program.append(.{ .Jmp = label_name_start });
        try s.program.append(.{ .Label = label_name_end });
    }
};

const ExpressionGenerator = struct {
    fn generate_rvalue(s: *ExecutionState, input_expr: ast.Expression) IRError!typing.TypeInfo {
        switch (input_expr) {
            .LiteralInt => |tk| {
                try s.program.append(.{ .PushW = @bitCast(tk.tag.Integer) });
                return typing.PinePrimitive.get("untyped_int").?;
            },
            .LiteralString => |tk| {
                try s.program.append(.{ .StaticAddr = s.static.items.len });
                try s.static.appendSlice(tk.tag.String);
                //TODO: this is for cstring and not having escape characters (some lexer fixies required)
                try s.static.append(10);
                try s.static.append(0);
                return typing.PinePrimitive.get("cstring").?;
            },
            .LiteralBool => |tk| {
                try s.program.append(.{ .PushB = if (tk.tag == .True) 1 else 0 });
                return typing.PinePrimitive.get("bool").?;
            },
            .IdentifierInvokation => |tk| {
                const info = try generate_lvalue(s, input_expr);
                try s.generate_mem_op(info, true, tk.location);
                return info;
            },
            .UnaryExpression => |expr| {
                switch (expr.op.tag) {
                    .Hat => {
                        const info = try ExpressionGenerator.generate_rvalue(s, expr.expr);
                        if (info.tag != .PinePtr) {
                            std.log.err("Cannot Dereference a non pointer {s}", .{expr.op});
                            return IRError.Syntax;
                        }
                        try s.generate_mem_op(info, true, expr.op.location);
                        return try s.types.from_ast(info.child.?);
                    },
                    .Ampersand => {
                        const info = try ExpressionGenerator.generate_lvalue(s, expr.expr);
                        //TODO: this is not the actual info it should be ptr to but typing does not support that
                        return info;
                    },
                    .Dash => {
                        try s.program.append(.{ .PushW = @bitCast(@as(isize, -1)) });
                        const info = try ExpressionGenerator.generate_rvalue(s, expr.expr);
                        try s.program.append(.{ .Mul_I = false });
                        try typing.equivalent(info, typing.PinePrimitive.get("integer").?);
                        return info;
                    },
                    .ExclamationMark => {
                        const info = try ExpressionGenerator.generate_rvalue(s, expr.expr);
                        try typing.equivalent(info, typing.PinePrimitive.get("bool").?);
                        try s.program.append(.Not);
                    },
                    else => unreachable,
                }
            },
            .BinaryExpression => |expr| {
                const lhs_info = try generate_rvalue(s, expr.lhs);
                const rhs_info = try generate_rvalue(s, expr.rhs);
                try typing.equivalent(rhs_info, lhs_info);
                switch (expr.op.tag) {
                    .Plus => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Add_F = false } else .{ .Add_I = false }),
                    .Asterisk => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Mul_F = false } else .{ .Mul_I = false }),
                    .Dash => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Add_F = true } else .{ .Add_I = true }),
                    .SlashForward => try s.program.append(if (lhs_info.tag == .PineFloat) .{ .Mul_F = true } else .{ .Mul_I = true }),
                    .Equal => {
                        try s.program.append(.Eq);
                        return typing.PinePrimitive.get("bool").?;
                    },
                    .NotEqual => {
                        try s.program.append(.Not);
                        try s.program.append(.Eq);
                        return typing.PinePrimitive.get("bool").?;
                    },
                    .LessThan => {
                        try s.program.append(if (lhs_info.tag == .PineFloat) .LT_F else .LT_I);
                        return typing.PinePrimitive.get("bool").?;
                    },
                    .GreaterThan => {
                        try s.program.append(if (lhs_info.tag == .PineFloat) .GT_F else .GT_I);
                        return typing.PinePrimitive.get("bool").?;
                    },
                    .LessThanEqual => {
                        try s.program.append(if (lhs_info.tag == .PineFloat) .GT_F else .GT_I);
                        try s.program.append(.Not);
                        return typing.PinePrimitive.get("bool").?;
                    },
                    .GreaterThanEqual => {
                        try s.program.append(if (lhs_info.tag == .PineFloat) .LT_F else .LT_I);
                        try s.program.append(.Not);
                        return typing.PinePrimitive.get("bool").?;
                    },
                    else => {},
                }
                return lhs_info;
            },
            .FunctionInvokation => |expr| {
                const info = s.types.find_function(expr.name_tk) orelse {
                    std.log.err("Undeclared function {s}", .{expr.name_tk});
                    return IRError.Undeclared;
                };

                if (!info.external) {
                    try s.program.append(.{ .Reserve = info.return_type.size });
                }

                if (expr.args_list) |args| {
                    if (args.len != info.params.len) {
                        std.log.err("Expected {d} parameters got {d} {s}", .{ info.params.len, args.len, expr.name_tk });
                    }
                    var expr_iter = std.mem.reverseIterator(args);
                    var expected_iter = std.mem.reverseIterator(info.params);
                    while (expr_iter.next()) |arg| {
                        const expr_info = try ExpressionGenerator.generate_rvalue(s, arg);
                        try typing.equivalent(expr_info, expected_iter.next().?);
                    }
                } else {
                    if (info.params.len != 0) {
                        std.log.err("Expected {d} parameters got {d} {s}", .{ info.params.len, 0, expr.name_tk });
                        return IRError.Syntax;
                    }
                }

                if (!info.external) {
                    try s.program.append(.{ .Call = .{ .name = expr.name_tk.tag.Identifier, .param_size = info.param_size } });
                } else {
                    try s.program.append(.{ .CCall = .{ .name = expr.name_tk.tag.Identifier, .info = info } });
                }
                return info.return_type;
            },
            .ArrayInitialization => |ordered_list| {
                var length: usize = 0;

                var element_type: ?typing.TypeInfo = null;
                var elem_iter = std.mem.reverseIterator(ordered_list);
                while (elem_iter.next()) |exp| {
                    length += 1;
                    if (element_type) |typ| {
                        const next_type = try generate_rvalue(s, exp);
                        try typing.equivalent(next_type, typ);
                    } else {
                        element_type = try generate_rvalue(s, exp);
                    }
                }
                return .{
                    .tag = .PineArray,
                    .size = length * element_type.?.size,
                    .child = null, //TODO: this is bad
                };
            },
            .RecordInitialization => |list| {
                const typ = s.types.custom_types.get(list.name_tk.tag.Identifier) orelse {
                    std.log.err("Record type {s} is undefined {s}", .{ list.name_tk.tag.Identifier, list.name_tk.location });
                    return IRError.Undeclared;
                };

                //this is awfully slow
                std.mem.reverse([]const u8, typ.tag.PineRecord.keys());
                outer: for (typ.tag.PineRecord.keys()) |field_name| {
                    for (list.fields) |ast_field| {
                        if (std.mem.eql(u8, ast_field.field.tag.Identifier, field_name)) {
                            //could do some type checking here
                            _ = try ExpressionGenerator.generate_rvalue(s, ast_field.expr);
                            continue :outer;
                        }
                    }
                    std.log.err("Did not initialize field {s} {s}", .{ field_name, list.name_tk.location });
                    return IRError.Syntax;
                }
                std.mem.reverse([]const u8, typ.tag.PineRecord.keys());
                return typ;
            },
            .AccessExpression => |expr| {
                switch (expr.rhs) {
                    .RangeExpression => |r_expr| {
                        const input_info = try ExpressionGenerator.generate_lvalue(s, expr.lhs);
                        if (input_info.tag != .PineArray and input_info.tag != .PineWidePointer) {
                            std.log.err("Cannot slice a type that is not an array or slice {s}", .{r_expr.op.location});
                            return IRError.Syntax;
                        }
                        //correct order is length then ptr so comptime swap these boys
                        const lvalue_inst = s.program.pop();
                        //a..b => b - a on the stack which is the length
                        _ = try ExpressionGenerator.generate_rvalue(s, r_expr.rhs);
                        _ = try ExpressionGenerator.generate_rvalue(s, r_expr.lhs);
                        try s.program.append(.{ .Add_I = true });
                        //add the lower bound to the start of the array ptr
                        try s.program.append(lvalue_inst);
                        _ = try ExpressionGenerator.generate_rvalue(s, r_expr.lhs);
                        const child = try s.types.from_ast(input_info.child.?);
                        try s.program.append(.{ .PushW = child.size });
                        try s.program.append(.{ .Mul_I = false });
                        try s.program.append(.{ .Add_I = false });

                        if (input_info.tag == .PineArray) {
                            return s.types.from_ast(input_info.child.?);
                        } else {
                            @panic("not implemented");
                        }
                    },
                    else => {},
                }
                const info = try ExpressionGenerator.generate_lvalue(s, input_expr);
                try s.generate_mem_op(info, true, expr.op.location);
                return info;
            },

            else => {},
        }
        unreachable;
    }

    fn generate_lvalue(s: *ExecutionState, input_expr: ast.Expression) IRError!typing.TypeInfo {
        switch (input_expr) {
            .IdentifierInvokation => |tk| {
                const info = try s.find_identifier(tk, s.scopes.top_idx());
                try s.push_variable_address(info);
                return info.type_info;
            },
            .AccessExpression => |expr| {
                const initial_info = try ExpressionGenerator.generate_lvalue(s, expr.lhs);
                const type_info = try ExpressionGenerator.generate_access(s, expr.rhs, initial_info);
                try s.program.append(.{ .Add_I = false });
                return type_info;
            },
            .UnaryExpression => |expr| {
                if (expr.op.tag != .Hat) {
                    std.log.err("Cannot use {s} as an lvalue", .{input_expr});
                    return IRError.Syntax;
                }
                const info = try generate_rvalue(s, expr.expr);
                return try s.types.from_ast(info.child.?);
            },
            else => {},
        }
        unreachable;
    }

    fn generate_access(ir: *ExecutionState, expr: ast.Expression, input_info: typing.TypeInfo) !typing.TypeInfo {
        var child: typing.TypeInfo = undefined;
        if (input_info.child) |ast_child| {
            child = try ir.types.from_ast(ast_child);
        }
        switch (expr) {
            .IdentifierInvokation => |field_tk| {
                switch (input_info.tag) {
                    .PineArray => {
                        _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                        try ir.program.append(.{ .PushW = child.size });
                        try ir.program.append(.{ .Mul_I = false });
                        return child;
                    },
                    .PineRecord => {
                        if (input_info.tag.PineRecord.get(field_tk.tag.Identifier)) |field_info| {
                            try ir.program.append(.{ .PushW = field_info.offset });
                            return field_info.type_info;
                        }
                        std.log.err("No field {s} in record", .{field_tk});
                        return IRError.Syntax;
                    },
                    .PineWidePointer => {
                        if (input_info.tag.PineRecord.get(field_tk.tag.Identifier)) |field_info| {
                            try ir.program.append(.{ .PushW = field_info.offset });
                            return field_info.type_info;
                        }
                        // try ir.program.append(.{ .PushW = 8 });
                        // try ir.program.append(.{ .Add_I = false });
                        try ir.program.append(.LoadW); //load data ptr
                        _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                        try ir.program.append(.{ .PushW = child.size });
                        try ir.program.append(.{ .Mul_I = false });
                        return child;
                    },
                    else => {
                        std.log.err("Cannot access variable (. operator) if it is not a record or array {s}", .{field_tk});
                        return IRError.Syntax;
                    },
                }
            },
            .AccessExpression => |a_expr| {
                switch (input_info.tag) {
                    .PineRecord, .PineWidePointer => {
                        const rhs_input_info = try generate_access(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access(ir, a_expr.rhs, rhs_input_info);
                        try ir.program.append(.{ .Add_I = false });
                        return rhs_info;
                    },
                    .PineArray => {
                        _ = try generate_access(ir, a_expr.lhs, input_info);
                        const rhs_info = try generate_access(ir, a_expr.rhs, child);
                        try ir.program.append(.{ .Add_I = false });
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
                if (input_info.tag != .PineArray and input_info.tag != .PineWidePointer) {
                    std.log.err("Tried to access a non indexible value {s}", .{expr});
                    return IRError.Syntax;
                }
                if (input_info.tag == .PineWidePointer) {
                    // try ir.program.append(.{ .StackAddr = 8 });
                    // try ir.program.append(.{ .Add_I = false });
                    try ir.program.append(.LoadW);
                    _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                    try ir.program.append(.{ .PushW = child.size });
                    try ir.program.append(.{ .Mul_I = false });
                    return child;
                } else {
                    _ = try ExpressionGenerator.generate_rvalue(ir, expr);
                    try ir.program.append(.{ .PushW = (try ir.types.from_ast(input_info.child.?)).size });
                    try ir.program.append(.{ .Mul_I = false });
                    return child;
                }
            },
        }
    }
};

fn print(ir: FileIR) void {
    for (ir.instructions) |inst| {
        std.debug.print("{any}\n", .{inst});
    }
}

//general purpose stack
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

fn scratch_interpret(ir: FileIR) void {
    var mem = [1]u64{0} ** 1064;
    var sp: usize = 0;
    for (ir.instructions) |inst| {
        switch (inst) {
            .PushW => |v| {
                mem[sp] = v;
                sp += 1;
            },
            .PushB => |v| {
                mem[sp] = v;
                sp += 1;
            },
            .StackAddr => |v| {
                mem[sp] = v;
                sp += 1;
            },
            .Add_I => |sub| {
                const a = mem[sp - 1];
                const b = mem[sp - 2];
                sp -= 2;
                if (sub) {
                    mem[sp] = b - a;
                } else {
                    mem[sp] = a + b;
                }
                sp += 1;
            },
            .Mul_I => |sub| {
                const a = mem[sp - 1];
                const b = mem[sp - 2];
                sp -= 2;
                if (sub) {
                    @panic("no divide pls");
                } else {
                    mem[sp] = a * b;
                }
                sp += 1;
            },
            .Reserve => |v| {
                sp += v;
            },
            .StoreW => {
                const addr = mem[sp - 1];
                const v = mem[sp - 2];
                sp -= 2;
                mem[addr] = v;
            },
            .StoreB => {
                const addr = mem[sp - 1];
                const v = mem[sp - 2];
                sp -= 2;
                mem[addr] = v;
            },
            .LoadW => {
                const addr = mem[sp - 1];
                sp -= 1;
                mem[sp] = mem[addr];
                sp += 1;
            },
            .LoadB => {
                const addr = mem[sp - 1];
                sp -= 1;
                mem[sp] = mem[addr];
                sp += 1;
            },
            .StaticAddr => {},
            .CCall => |info| {
                sp -= info.info.params.len;
            },
            else => {},
        }
    }
    for (0..63) |i| {
        if (i % 8 == 0) std.debug.print("\n", .{});
        std.debug.print("{d} ", .{mem[i]});
    }
}
