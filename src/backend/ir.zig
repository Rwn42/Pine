const std = @import("std");

const ast = @import("../frontend/ast.zig");
const TranslationUnit = @import("../compile.zig").TranslationUnit;
const typing = @import("./typing.zig");
const Stack = @import("../common.zig").Stack;
const Token = @import("../frontend/token.zig").Token;

const IRError = error{
    TypeMismatch,
    Undeclared,
    Duplicate,
    OutOfmemory,
};

pub const Instructions = union(enum) {
    LiteralValue: u64,
    StackAddr: u64, //offset
    OpenFunction: .{ []u8, usize }, //name and stack size
    CloseFunction,
    OpenBlock,
    StaticMem: []u8,
    Call: []u8,
    Load: usize, //size of load
    Store: usize, //size of store
    Addi,
    Addf,
    Muli,
    Mulf,
};

const VarInfo = struct {
    stack_offset: usize,
    type_info: typing.TypeInfo,
};

const IRGenerator = struct {
    const Self = @This();

    program_buffer: std.ArrayList(Instructions),
    unit: TranslationUnit,

    stack_pointer: usize = 0,
    open_blocks: Stack(std.StringHashMap(VarInfo)),

    fn find_identifier(self: *Self, identifier_tk: Token, idx: usize) !VarInfo {
        const identifier = identifier_tk.tag.Identifier;
        if (self.open_blocks.get(idx).get(identifier)) |info| return info;
        if (idx == 0) {
            std.log.err("Undeclared identifier {s}", .{identifier_tk});
            return IRError.Undeclared;
        }
        return try self.find_identifier(identifier_tk, idx - 1);
    }

    fn register_var(self: *Self, name_tk: Token, typ: typing.TypeInfo) !VarInfo {
        if (self.open_blocks.top().contains(name_tk.tag.Identifier)) {
            std.log.err("Duplicate definition of identifier {s}", .{name_tk});
            return IRError.Duplicate;
        }
        try self.open_blocks.top().put(name_tk.tag.Identifier, .{
            .type_info = typ,
            .stack_offset = self.stack_pointer,
        });
        self.stack_pointer += typ.size;
        return self.scope.top().identifiers.get(name_tk.tag.Identifier).?;
    }

    fn generate_function(self: *Self, f_decl: *ast.FunctionDeclarationNode) !void {
        var open_function_idx = self.program_buffer.items.len;
        try self.program_buffer.append(.{ .OpenFunction = .{ f_decl.name_tk.tag.Identifier, 0 } });

        //NOTE: do not need to parse type again its alreayd done in Type struct

        //at the end of this we know the stack size so we edit the open function inst from the function start
        self.program_buffer.items[open_function_idx].OpenFunction[1] = self.stack_pointer;
    }
};

pub fn generate_ir(tu: TranslationUnit, functions: []*ast.FunctionDeclarationNode, allocator: std.mem.Allocator) ![]Instructions {
    var generator = IRGenerator{
        .program_buffer = std.ArrayList(Instructions).init(allocator),
        .unit = tu,
        .open_blocks = Stack(std.StringHashMap(VarInfo)).init(),
    };

    for (functions) |f_decl| {
        try generator.generate_function(f_decl);
    }
}
