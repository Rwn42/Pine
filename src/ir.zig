const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");
const typing = @import("typing.zig");

const Opcode = enum {
    istore,
    fstore,
    bstore,
    iload,
    fload,
    bload,
    addi,
    addf,
    addb,
    muli,
    mulf,
    mulb,
    ipush,
    fpush,
    bpush,
};

const Operand = union {
    byte: u8,
    integer: i64,
    float: f64,
};

const Operation = struct {
    opc: Opcode,
    operand: Operand,
};

const Scope = struct {
    level: isize,
};

pub const IRGenerator = struct {
    const Self = @This();

    top_level: []AST.Declaration,
    scope_stack: std.ArrayList(Scope),
    program: std.ArrayList(Operation),
    tm: typing.TypeManager,

    pub fn init(top_level: []AST.Declaration, allocator: std.mem.Allocator) Self {
        return .{
            .top_level = top_level,
            .scope_stack = std.ArrayList(Scope).init(allocator),
            .program = std.ArrayList(Operation).initCapacity(allocator, 100) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            },
            .tm = typing.TypeManager.init(allocator),
        };
    }

    pub fn generate(self: *Self) !void {
        for (self.top_level) |decl| {
            switch (decl) {
                .RecordDeclaration => |recordNode| {
                    try self.tm.register_record(recordNode);
                },
                else => @panic("not implemented"),
            }
        }
    }

    pub fn deinit(self: *Self) void {
        self.scope_stack.deinit();
        self.program.deinit();
    }
};
