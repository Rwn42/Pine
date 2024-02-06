const std = @import("std");

const AST = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;

const TypeError = error{
    OutOfMemory,
    Undeclared,
    Duplicate,
};

pub const FunctionType = struct {
    param_types: []TypeInfo,
    return_type: TypeInfo,
};

pub const TypeTag = enum {
    Record,
    WidePointer,
    Array,
    Pointer,

    Integer,
    Float,
    Bool,
    Byte,
    Word,
    Void,
    UntpyedInt,
};

pub const BasicTypeData = struct {
    tag: TypeTag,
    size: usize,
};

const RecordFields = std.StringArrayHashMap(TypeInfo);

const ChildInfo = union {
    type_info: *TypeInfo,
    field_info: *RecordFields,
    type_id: usize,

    fn size(child: ChildInfo, types: Types) usize {
        switch (child) {
            .type_info => |info| return info.size,
            .type_id => |id| return types.custom_types.values()[id].size,
            .field_info => @panic("COMPILER ERROR: tried to access size of record field map"),
        }
    }
};

pub const TypeInfo = struct {
    tag: TypeTag,
    size: usize,
    child: ?ChildInfo,
};

pub const Types = struct {
    const Self = @This();

    pub const TypeQueryResult = union {
        type_id: usize,
        type_info: TypeInfo,
    };

    pub const Primitive = std.ComptimeStringMap(TypeInfo, .{
        .{ "int", .{ .size = 8, .tag = .Integer, .child = null } },
        .{ "float", .{ .size = 8, .tag = .Float, .child = null } },
        .{ "bool", .{ .size = 1, .tag = .Bool, .child = null } },
        .{ "byte", .{ .size = 1, .tag = .Byte, .child = null } },
        .{ "void", .{ .size = 0, .tag = .Void, .child = null } },
        .{ "untyped_int", .{ .size = 8, .tag = .UntypedInt, .child = null } },
    });

    arena: std.heap.ArenaAllocator,
    custom_types: std.StringArrayHashMap(TypeInfo),
    function_types: std.StringHashMap(FunctionType),

    pub fn from_ast(self: *Self, dt: AST.DefinedType) TypeError!TypeQueryResult {
        switch (dt) {
            .Basic => |typ| {
                if (Primitive.get(typ.tag.Identifier)) |info| return .{ .type_info = info };
                if (self.custom_types.get(typ.tag.Identifier)) |info| return .{ .type_id = info };
                std.log.err("Undeclared type {s}", .{typ});
                return TypeError.Undeclared;
            },
            .Pointer => |p| {
                const child_info = try self.from_ast(p.pointing_to);
                const child: ChildInfo = switch (child_info) {
                    .type_id => |id| .{ .type_id = id },
                    .type_info => |info| .{ .type_info = self.new_from_info(info) },
                };
                return .{ .size = 8, .tag = .Pointer, .child = child };
            },
            .Array => |arr| {
                //TODO: implement array lenght expressios
                const length: u64 = @intCast(arr.length.LiteralInt.tag.Integer);
                const child_info = try self.from_ast(arr.element_typ);
                const child: ChildInfo = switch (child_info) {
                    .type_id => |id| .{ .type_id = id },
                    .type_info => |info| .{ .type_info = self.new_from_info(info) },
                };
                return .{ .size = length * child.size(self), .tag = .Pointer, .child = child };
            },
            .WidePointer => |wp| {
                const child_info = try self.from_ast(wp.pointing_to);
                const child: ChildInfo = switch (child_info) {
                    .type_id => |id| .{ .type_id = id },
                    .type_info => |info| .{ .type_info = self.new_from_info(info) },
                };
                return .{ .size = 16, .tag = .WidePointer, .child = child };
            },
        }
    }

    fn new_info(self: *Self) *TypeInfo {
        return self.arena.allocator().create(TypeInfo) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }
    fn new_from_info(self: *Self, t_info: TypeInfo) *TypeInfo {
        var info = self.arena.allocator().create(TypeInfo) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
        info.* = t_info;
        return info;
    }
};
