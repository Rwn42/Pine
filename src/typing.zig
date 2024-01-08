const std = @import("std");

const AST = @import("ast.zig");
const IRError = @import("ir.zig").IRError;

const IdTypeMap = std.StringHashMap(TypeInfo);

pub const Primitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "int", .{ .size = 8, .tag = .Integer, .child = null } },
    .{ "float", .{ .size = 8, .tag = .Float, .child = null } },
    .{ "bool", .{ .size = 1, .tag = .Bool, .child = null } },
    .{ "byte", .{ .size = 1, .tag = .Byte, .child = null } },
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

const FieldInfo = struct { nothing: ?u8 };

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
        self.procedures.deinit();
    }

    pub fn generate(self: Self, dt: AST.DefinedType) !TypeInfo {
        return switch (dt) {
            .Basic => |typ| {
                if (Primitive.get(typ.tag.Identifier)) |info| return info;
                if (self.records.get(typ.tag.Identifier)) |info| return info;
                std.log.err("Undeclared type {t}", .{typ});
                return IRError.Undeclared;
            },
            .Pointer => |p| .{
                .size = 8,
                .tag = .Pointer,
                .child = self.generate(p.pointing_to),
            },
            else => @panic("Array Not Implemented"),
        };
    }

    pub fn register_record(self: *Self, decl: *AST.RecordDeclarationNode) !void {
        _ = self; // autofix
        _ = decl; // autofix
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
};

pub const TypeInfo = struct {
    const Child = union {
        feild_info: FieldInfo,
        type_info: *TypeInfo,
    };

    size: usize,
    tag: TypeTag,
    child: ?Child,
};