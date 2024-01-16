const std = @import("std");

const Token = @import("../frontend/token.zig").Token;
const TokenType = @import("../frontend/token.zig").TokenType;
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

pub const IRGenerator = struct {
    const VarInfo = struct {
        stack_loc: usize,
        type_info: typing.TypeInfo,
    };

    const Scope = struct {
        identifiers: std.StringHashMap(VarInfo),
        start_ip: usize,
        end_ip: usize,
    };

    const Self = @This();
    const ScopeStackType = Stack(Scope, 32, "Too many nested blocks limit is 30");

    program: std.ArrayList(Operation),
    allocator: std.mem.Allocator,
    scope_stack: ScopeStackType,
    tm: typing.TypeManager,

    pub fn init(allocator: std.mem.Allocator, declarations: []AST.Declaration) !Self {
        var tm = typing.TypeManager.init(allocator);
        for (declarations) |decl| {
            switch (decl) {
                .FunctionDeclaration => |f_decl| try tm.register_function(f_decl),
                .RecordDeclaration => |r_decl| try tm.register_record(r_decl),
                .ConstantDeclaration => @panic("Not implemented"),
                //eventually constants go to comptime eval and end up as map of string -> value
            }
        }
        return .{
            .program = std.ArrayList(Operation).init(allocator),
            .allocator = allocator,
            .scope_stack = ScopeStackType.init(),
            .tm = tm,
        };
    }

    pub fn generate_program(self: *Self) ![]Operation {
        defer self.deinit();
        return self.program.toOwnedSlice();
    }

    pub fn deinit(self: *Self) void {
        self.tm.deinit();
    }
};
