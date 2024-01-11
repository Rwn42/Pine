const std = @import("std");

const AST = @import("ast.zig");
const IRError = @import("ir.zig").IRError;

const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;

const IdTypeMap = std.StringHashMap(TypeInfo);

pub const Primitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "int", .{ .size = 64, .tag = .Integer, .child = null } },
    .{ "float", .{ .size = 64, .tag = .Float, .child = null } },
    .{ "bool", .{ .size = 8, .tag = .Bool, .child = null } },
    .{ "byte", .{ .size = 8, .tag = .Byte, .child = null } },
});

pub const TypeTag = enum {
    Pointer,
    Array,
    Record,
    Integer,
    Byte,
    Float,
    Bool,
    String,
};

const FieldInfo = struct {
    identifier: Token,
    type_info: TypeInfo,
    offset: usize,
    next: ?*FieldInfo,
};

pub const TypeManager = struct {
    const Self = @This();

    records: IdTypeMap,
    functions: IdTypeMap,
    arena: std.heap.ArenaAllocator, //stores typeinfo allocations for record fields

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .records = IdTypeMap.init(allocator),
            .functions = IdTypeMap.init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.records.deinit();
        self.functions.deinit();
    }

    pub fn generate(self: *Self, dt: AST.DefinedType) !TypeInfo {
        return switch (dt) {
            .Basic => |typ| {
                if (Primitive.get(typ.tag.Identifier)) |info| return info;
                if (self.records.get(typ.tag.Identifier)) |info| return info;
                std.log.err("Undeclared type {t}", .{typ});
                return IRError.Undeclared;
            },
            .Pointer => |p| blk: {
                var child = self.new_info();
                child.* = try self.generate(p.pointing_to);
                break :blk .{
                    .size = 64,
                    .tag = .Pointer,
                    .child = .{ .type_info = child },
                };
            },
            .Array => |arr| blk: {
                if (!TokenType.eq(arr.length.tag, .{ .Integer = 0 })) @panic("Const array lenght not implemented");
                var child = self.new_info();
                child.* = try self.generate(arr.element_typ);
                break :blk .{
                    .size = child.size * @as(usize, @intCast(arr.length.tag.Integer)),
                    .tag = .Array,
                    .child = .{ .type_info = child },
                };
            },
        };
    }

    pub fn register_record(self: *Self, decl: *AST.RecordDeclarationNode) !void {
        var record = TypeInfo{
            .size = undefined,
            .child = .{ .field_info = null },
            .tag = .Record,
        };

        var cur_offset: usize = 0;
        var dt_field = decl.fields;
        var prev: *?*FieldInfo = &record.child.?.field_info;
        while (dt_field) |field| {
            const field_type = try self.generate(field.typ);
            record.size += field_type.size;

            const new_field_info = self.arena.allocator().create(FieldInfo) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };

            new_field_info.type_info = field_type;
            new_field_info.offset = cur_offset;
            new_field_info.identifier = field.name_tk;
            new_field_info.next = null;

            prev.* = new_field_info;

            cur_offset += field_type.size;
            dt_field = field.next;
        }

        if (self.records.contains(decl.name_tk.tag.Identifier)) {
            std.log.err("Duplicate record definition {s}", .{decl.name_tk});
            return IRError.DuplicateDefinition;
        }
        self.records.put(decl.name_tk.tag.Identifier, record) catch {
            @panic("FATAL COMPILER ERROR: Out of memory!");
        };
    }

    pub fn register_function(self: *Self, decl: *AST.FunctionDeclarationNode) !void {
        if (self.functions.contains(decl.name_tk.tag.Identifier)) {
            std.log.err("Duplicate function definition {s}", .{decl.name_tk});
            return IRError.DuplicateDefinition;
        }
        const ret_type_info = try self.generate(decl.return_typ);
        self.functions.put(decl.name_tk.tag.Identifier, ret_type_info) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    fn new_info(self: *Self) *TypeInfo {
        return self.arena.allocator().create(TypeInfo) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }
};

pub const TypeInfo = struct {
    const Child = union {
        field_info: ?*FieldInfo,
        type_info: *TypeInfo,
    };

    size: usize,
    tag: TypeTag,
    child: ?Child,
};
