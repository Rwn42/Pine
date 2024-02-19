const std = @import("std");

const ast = @import("../frontend/ast.zig");

const Token = @import("../frontend/token.zig").Token;
const TokenTag = @import("../frontend/token.zig").TokenTag;
const FileLocation = @import("../frontend/token.zig").FileLocation;
const IRError = @import("ir.zig").IRError;

const ByteToken = Token{
    .location = .{ .row = 0, .col = 0, .filename = "compiler_generated" },
    .tag = .{ .Identifier = "byte" },
};

pub const PinePrimitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "int", .{ .size = 8, .tag = .PineInt, .child = null } },
    .{ "float", .{ .size = 8, .tag = .PineFloat, .child = null } },
    .{ "word", .{ .size = 8, .tag = .PineWord, .child = null } },
    .{ "bool", .{ .size = 1, .tag = .PineBool, .child = null } },
    .{ "byte", .{ .size = 1, .tag = .PineByte, .child = null } },
    .{ "void", .{ .size = 0, .tag = .PineVoid, .child = null } },
    .{ "string", .{ .size = 16, .tag = .PineWidePointer, .child = .{ .Basic = ByteToken } } },
    .{ "cstring", .{ .size = 8, .tag = .PinePtr, .child = .{ .Basic = ByteToken } } },
    .{ "untyped_int", .{ .size = 8, .tag = .PineUntypedInt, .child = null } },
});

const Fields = std.StringArrayHashMap(FieldInfo);

pub const FieldInfo = struct {
    type_info: TypeInfo,
    offset: usize,
};

pub const FuncInfo = struct {
    params: []TypeInfo,
    public: bool,
    return_type: TypeInfo,

    //should put this in the struct too lazy tho
    pub fn param_size(f: FuncInfo) usize {
        var size: usize = 0;
        for (f.params) |p| size += p.size;
        return size;
    }
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

    pub fn is_trivial(t: TypeTag) bool {
        switch (t) {
            .PineWidePointer, .PineRecord, .PineArray => false,
            else => true,
        }
    }
};

pub const TypeInfo = struct {
    size: usize,
    tag: TypeTag,
    child: ?ast.DefinedType,
};

//TODO: Implement
pub fn equivalent(first: TypeInfo, second: TypeInfo) !void {
    _ = first;
    _ = second;
    return;
}

pub const FileTypes = struct {
    const Self = @This();

    custom_types: std.StringArrayHashMap(TypeInfo),
    function_types: std.StringArrayHashMap(FuncInfo),
    public: std.StringHashMap(void),

    imported_types: []*const FileTypes,
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .custom_types = std.StringArrayHashMap(TypeInfo).init(allocator),
            .function_types = std.StringArrayHashMap(FuncInfo).init(allocator),
            .imported_types = undefined,
            .public = std.StringHashMap(void).init(allocator),
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn from_ast(self: *const Self, ast_type: ast.DefinedType) !TypeInfo {
        switch (ast_type) {
            .Basic => |typ| {
                if (PinePrimitive.get(typ.tag.Identifier)) |info| return info;
                if (self.custom_types.get(typ.tag.Identifier)) |info| return info;
                for (self.imported_types) |file_types| {
                    if (file_types.custom_types.get(typ.tag.Identifier)) |info| {
                        if (!file_types.public.contains(typ.tag.Identifier)) {
                            std.log.err("Tried to use non public type {s}", .{typ});
                            return IRError.Undeclared;
                        }
                        return info;
                    }
                }
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

    pub fn find_function(self: *const Self, name_tk: Token) ?FuncInfo {
        const name = name_tk.tag.Identifier;
        if (self.function_types.get(name)) |info| return info;
        for (self.imported_types) |imported| {
            for (imported.function_types.values(), imported.function_types.keys()) |v, k| {
                if (std.mem.eql(u8, name, k) and v.public) return v;
            }
        }
        return null;
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
            const key = try self.arena.allocator().dupe(u8, ast_field.name_tk.tag.Identifier);
            try map.put(key, new_fieldinfo);

            cur_offset += field_ti.size;
        }
        const key = try self.arena.allocator().dupe(u8, decl.name_tk.tag.Identifier);

        if (decl.public) try self.public.put(key, {});
        try self.custom_types.put(key, .{ .size = cur_offset, .tag = .{ .PineRecord = map }, .child = null });
    }

    pub fn register_function(self: *Self, decl: *ast.FunctionDeclarationNode) IRError!void {
        var function_type = FuncInfo{ .public = decl.public, .params = undefined, .return_type = undefined };

        var param_builder = std.ArrayList(TypeInfo).init(self.allocator);
        defer param_builder.deinit();
        for (decl.params) |p| {
            const param_info = try self.from_ast(p.typ);
            try param_builder.append(param_info);
        }
        function_type.params = try param_builder.toOwnedSlice();

        function_type.return_type = PinePrimitive.get("void").?;
        if (decl.return_typ) |typ| function_type.return_type = try self.from_ast(typ);

        const key = try self.arena.allocator().dupe(u8, decl.name_tk.tag.Identifier);
        if (decl.public) try self.public.put(key, {});
        try self.function_types.put(key, function_type);
    }

    pub fn deinit(self: *Self) void {
        for (self.custom_types.values()) |typ| {
            switch (typ.tag) {
                .PineRecord => |fields| fields.deinit(),
                else => {},
            }
        }
        for (self.function_types.values()) |function_type| {
            self.allocator.free(function_type.params);
        }
        self.custom_types.deinit();
        self.function_types.deinit();
        self.allocator.free(self.imported_types);
        self.public.deinit();
        self.arena.deinit();
    }
};
