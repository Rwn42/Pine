const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const AST = @import("ast.zig");

const Primitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "byte", .{ .size = 1, .data = .Byte } },
    .{ "int", .{ .size = 8, .data = .Integer } },
    .{ "float", .{ .size = 8, .data = .Float } },
    .{ "bool", .{ .size = 1, .data = .Boolean } },
});

pub const TypeError = error{
    UnknownType,
    DuplicateType,
};

pub const TypeManager = struct {
    const Self = @This();

    records: std.StringHashMap(TypeInfo),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .records = std.StringHashMap(TypeInfo).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn register_record(self: *Self, record: *AST.RecordDeclarationNode) !void {
        if (self.records.get(record.name_tk.tag.Identifier) != null) {
            std.log.err("Duplicate definition of record {s} duplicate definition here {s}", .{
                record.name_tk.tag.Identifier, record.name_tk.loc,
            });
            return TypeError.DuplicateType;
        }
        const name = record.name_tk.tag.Identifier;
        var field_data = std.StringArrayHashMap(FieldInfo).init(self.allocator);
        const size = try self.add_record_field(&field_data, record.fields.?, 0);

        const info = TypeInfo{ .size = size, .data = .{ .Record = field_data } };

        try self.records.put(name, info);
    }

    fn add_record_field(self: *Self, hashmap: *std.StringArrayHashMap(FieldInfo), node: *AST.ParamList, offset: usize) !usize {
        const typ = try self.get(node.typ);
        const field_info = FieldInfo{ .offset = offset, .type_info = typ };
        hashmap.put(node.name_tk.tag.Identifier, field_info) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };

        if (node.next) |next| return typ.size + try self.add_record_field(hashmap, next, offset + typ.size);
        return typ.size;
    }

    pub fn get(self: Self, defined_type: AST.DefinedType) TypeError!TypeInfo {
        switch (defined_type) {
            .Basic => |tk| {
                const o_type_info = Primitive.get(tk.tag.Identifier) orelse self.records.get(tk.tag.Identifier);
                if (o_type_info) |type_info| return type_info;
                std.log.err("Type {tk} is not defined", .{tk});
                return TypeError.UnknownType;
            },
            else => {
                @panic("Not implemetned");
            },
        }
    }
};

pub const TypeData = union(enum) {
    Float,
    Integer,
    Boolean,
    Byte,
    Record: std.StringArrayHashMap(FieldInfo),
    Array: *ArrayData,
    Pointer: *TypeInfo,
};

pub const ArrayData = struct {
    length: usize,
    element: TypeInfo,
};

const FieldInfo = struct {
    type_info: TypeInfo,
    offset: usize,
};

pub const TypeInfo = struct {
    size: usize,
    data: TypeData,
};
