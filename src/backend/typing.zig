const std = @import("std");

const AST = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;

pub const TypeError = error{
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
    UntypedInt,
};

const RecordFields = std.StringArrayHashMap(FieldInfo);

const ChildInfo = union(enum) {
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

pub const FieldInfo = struct {
    type_info: TypeInfo,
    offset: usize,
};

pub const Types = struct {
    const Self = @This();

    pub const TypeQueryResult = union(enum) {
        type_id: usize,
        type_info: TypeInfo,

        fn unwrap(result: TypeQueryResult, types: Types) TypeInfo {
            switch (result) {
                .type_info => |info| return info,
                .type_id => |id| return types.custom_types.values()[id],
            }
        }
    };

    pub const Primitive = std.ComptimeStringMap(TypeInfo, .{
        .{ "int", .{ .size = 8, .tag = .Integer, .child = null } },
        .{ "float", .{ .size = 8, .tag = .Float, .child = null } },
        .{ "word", .{ .size = 8, .tag = .Word, .child = null } },
        .{ "bool", .{ .size = 1, .tag = .Bool, .child = null } },
        .{ "byte", .{ .size = 1, .tag = .Byte, .child = null } },
        .{ "void", .{ .size = 0, .tag = .Void, .child = null } },
        .{ "untyped_int", .{ .size = 8, .tag = .UntypedInt, .child = null } },
    });

    arena: std.heap.ArenaAllocator,
    custom_types: std.StringArrayHashMap(TypeInfo),
    function_types: std.StringHashMap(FunctionType),

    pub fn init(allocator: std.mem.Allocator) Types {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .custom_types = std.StringArrayHashMap(TypeInfo).init(allocator),
            .function_types = std.StringHashMap(FunctionType).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.custom_types.deinit();
        self.function_types.deinit();
        self.arena.deinit();
    }

    //should work still does not work for records defined in bad order
    pub fn register_record(self: *Self, r_decl: AST.RecordDeclarationNode) !void {
        const record_name = r_decl.name_tk.tag.Identifier;
        if (self.custom_types.contains(record_name)) {
            std.log.err("Duplicate definition of {s}", .{r_decl.name_tk});
            return TypeError.Duplicate;
        }

        const record_id = self.custom_types.values().len; //used for self reference
        try self.custom_types.put(record_name, undefined);

        const fields = try self.arena.allocator().create(RecordFields);
        fields.* = RecordFields.init(self.arena.allocator());

        var record: TypeInfo = .{
            .size = 0,
            .tag = .Record,
            .child = .{ .field_info = fields },
        };

        for (r_decl.fields) |ast_param| {
            const field_type_result = try self.from_ast(ast_param.typ);
            switch (field_type_result) {
                .type_id => |id| if (id == record_id) {
                    std.log.err("Self referential type defined here {s}", .{ast_param.name_tk});
                    return TypeError.Duplicate;
                },
                else => {},
            }
            const field_type = field_type_result.unwrap(self.*);
            if (record.child.?.field_info.contains(ast_param.name_tk.tag.Identifier)) {
                std.log.err("Duplicate field {s}", .{ast_param.name_tk});
                return TypeError.Duplicate;
            }
            try record.child.?.field_info.put(
                ast_param.name_tk.tag.Identifier,
                .{ .type_info = field_type, .offset = record.size },
            );
            record.size += field_type.size;
        }

        try self.custom_types.put(record_name, record);
    }

    pub fn register_function(self: *Self, f_decl: AST.FunctionDeclarationNode) !void {
        const name = f_decl.name_tk.tag.Identifier;
        if (self.function_types.contains(name)) {
            std.log.err("Duplicate function definition here {s}", .{name});
            return TypeError.Duplicate;
        }

        const return_type = try self.from_ast(f_decl.return_typ);

        var param_type_builder = std.ArrayList(TypeInfo).init(self.arena.allocator());
        for (f_decl.params) |param| {
            try param_type_builder.append((try self.from_ast(param.typ)).unwrap(self.*));
        }

        try self.function_types.put(name, .{
            .return_type = return_type.unwrap(self.*),
            .param_types = try param_type_builder.toOwnedSlice(),
        });
    }

    pub fn from_ast(self: *Self, dt: AST.DefinedType) TypeError!TypeQueryResult {
        switch (dt) {
            .Basic => |typ| {
                if (Primitive.get(typ.tag.Identifier)) |info| return .{ .type_info = info };
                if (self.custom_types.getIndex(typ.tag.Identifier)) |info| return .{ .type_id = info };
                std.log.err("Undeclared type {s}", .{typ});
                return TypeError.Undeclared;
            },
            .Pointer => |p| {
                const child_info = try self.from_ast(p.pointing_to);
                const child: ChildInfo = switch (child_info) {
                    .type_id => |id| .{ .type_id = id },
                    .type_info => |info| .{ .type_info = self.new_from_info(info) },
                };
                return .{ .type_info = .{ .size = 8, .tag = .Pointer, .child = child } };
            },
            .Array => |arr| {
                //TODO: implement array lenght expressios
                const length: u64 = @intCast(arr.length.LiteralInt.tag.Integer);
                const child_info = try self.from_ast(arr.element_typ);
                const child: ChildInfo = switch (child_info) {
                    .type_id => |id| .{ .type_id = id },
                    .type_info => |info| .{ .type_info = self.new_from_info(info) },
                };
                return .{ .type_info = .{ .size = length * child.size(self.*), .tag = .Pointer, .child = child } };
            },
            .WidePointer => |wp| {
                const child_info = try self.from_ast(wp.pointing_to);
                const child: ChildInfo = switch (child_info) {
                    .type_id => |id| .{ .type_id = id },
                    .type_info => |info| .{ .type_info = self.new_from_info(info) },
                };
                return .{ .type_info = .{ .size = 16, .tag = .WidePointer, .child = child } };
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
