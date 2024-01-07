const std = @import("std");

const AST = @import("ast.zig");

const Primitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "int", .{ .size = 8, .is_float = false } },
    .{ "float", .{ .size = 8, .is_float = true } },
    .{ "bool", .{ .size = 1, .is_float = false } },
    .{ "byte", .{ .size = 1, .is_float = false } },
});

pub const TypeInfo = struct {
    size: usize,
    is_float: bool, //used so we can determine weather to invoke i_add or f_add like instructions

    pub fn gen(dt: AST.DefinedType) !TypeInfo {
        return switch (dt) {
            .Basic => |typ| Primitive.get(typ.tag.Identifier) orelse @panic("Record Not Implemented"),
            .Pointer => .{ .size = 8, .is_float = false },
            else => @panic("Not Implemented"),
        };
    }
};
