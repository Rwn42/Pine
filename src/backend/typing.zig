const std = @import("std");

const ast = @import("../frontend/ast.zig");

const Token = @import("../frontend/token.zig").Token;
const TokenTag = @import("../frontend/token.zig").TokenTag;
const FileLocation = @import("../frontend/token.zig").FileLocation;
const IRError = @import("ir.zig").IRError;

pub const PinePrimitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "int", .{ .size = 8, .tag = .PineInt, .child = null } },
    .{ "float", .{ .size = 8, .tag = .PineFloat, .child = null } },
    .{ "word", .{ .size = 8, .tag = .PineWord, .child = null } },
    .{ "bool", .{ .size = 1, .tag = .PineBool, .child = null } },
    .{ "byte", .{ .size = 1, .tag = .PineByte, .child = null } },
    .{ "void", .{ .size = 0, .tag = .PineVoid, .child = null } },
});

const Fields = std.StringArrayHashMap(FieldInfo);

pub const FieldInfo = struct {
    type_info: TypeInfo,
    offset: usize,
};

pub const FuncInfo = struct {
    params: []TypeInfo,
    return_type: ?TypeInfo,
};

pub const TypeTag = union(enum) {
    PineInt,
    PineFloat,
    PineByte,
    PinePtr,
    PineBool,
    PineVoid,
    PineWord,
    PineUntypedInt,
    PineWidePointer,
    PineRecord: *Fields,
    PineArray,
};

pub const TypeInfo = struct {
    size: usize,
    tag: TypeTag,
    child: ?ast.DefinedType,
};

pub const Types = struct {
    const Self = @This();

    custom_types: std.StringArrayHashMap(TypeInfo),
    function_types: std.StringArrayHashMap(FuncInfo),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .custom_types = std.StringArrayHashMap(TypeInfo).init(allocator),
            .function_types = std.StringArrayHashMap(FuncInfo).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn from_ast(self: *Self, ast_type: ast.DefinedType) !TypeInfo {
        switch (ast_type) {
            .Basic => |typ| {
                if (PinePrimitive.get(typ.tag.Identifier)) |info| return info;
                if (self.custom_types.get(typ.tag.Identifier)) |info| return info;
                std.log.err("Undeclared type {s}", .{typ});
                return IRError.Undeclared;
            },
            .Pointer => |p| {
                return .{ .size = 8, .tag = .PinePtr, .child = p.pointing_to };
            },
            .Array => |arr| {
                //TODO: comptime arr length expression
                const length = @as(usize, @intCast(arr.length.LiteralInt.tag.Integer));
                const child = try self.from_ast(arr.element_typ);
                return .{ .size = length * child.size, .tag = .PineArray, .child = arr.element_typ };
            },
            .WidePointer => |wp| {
                return .{ .size = 16, .tag = .PineWidePointer, .child = wp.pointing_to };
            },
        }
    }

    pub fn register_record(self: *Self, decl: *ast.RecordDeclarationNode) IRError!void {
        const map = try self.allocator.create(Fields);
        map.* = Fields.init(self.allocator);

        var cur_offset: usize = 0;
        for (decl.fields) |ast_field| {
            switch (ast_field.typ) {
                .Basic => |tk| {
                    if (std.mem.eql(u8, tk.tag.Identifier, decl.name_tk.tag.Identifier)) {
                        std.log.err("Recursive data structure defined here {s}", .{tk});
                        return IRError.Duplicate;
                    }
                },
                else => {},
            }

            const field_ti = try self.from_ast(ast_field.typ);

            const new_fieldinfo = FieldInfo{
                .offset = cur_offset,
                .type_info = field_ti,
            };

            try map.put(ast_field.name_tk.tag.Identifier, new_fieldinfo);

            cur_offset += field_ti.size;
        }

        try self.custom_types.put(decl.name_tk.tag.Identifier, .{ .size = cur_offset, .tag = .{ .PineRecord = map }, .child = null });
    }

    pub fn register_function(self: *Self, decl: *ast.FunctionDeclarationNode) IRError!void {
        _ = self;
        _ = decl;
    }

    pub fn deinit(self: *Self) void {
        for (self.custom_types.values()) |typ| {
            switch (typ.tag) {
                .PineRecord => |fields| fields.deinit(),
                else => {},
            }
        }
        self.custom_types.deinit();
        self.function_types.deinit();
    }
};
