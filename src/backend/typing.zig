const std = @import("std");

const AST = @import("../frontend/ast.zig");
const IRError = @import("ir.zig").IRError;
const ir = @import("ir.zig");

const Location = @import("../common.zig").Location;

const TokenType = @import("../frontend/token.zig").TokenType;
const Token = @import("../frontend/token.zig").Token;

const IdTypeMap = std.StringHashMap(TypeInfo);

//NOTE: A string is a wide pointer (slice) of bytes

pub const Primitive = std.ComptimeStringMap(TypeInfo, .{
    .{ "int", .{ .size = 8, .tag = .Integer, .child = null } },
    .{ "float", .{ .size = 8, .tag = .Float, .child = null } },
    .{ "bool", .{ .size = 1, .tag = .Bool, .child = null } },
    .{ "byte", .{ .size = 1, .tag = .Byte, .child = null } },
    .{ "void", .{ .size = 0, .tag = .Void, .child = null } },
    .{ "untyped_int", .{ .size = 8, .tag = .UntypedInt, .child = null } },
});

pub const TypeTag = enum {
    Pointer,
    Array,
    WidePointer,
    UntypedInt, //can be assigned to integer or byte or pointer maybe
    Record,
    Integer,
    Byte,
    Float,
    Bool,
    Void,
};

pub const FieldInfoStruct = struct {
    type_info: TypeInfo,
    offset: usize,
};

const FieldInfo = std.StringArrayHashMap(FieldInfoStruct);

pub const TypeManager = struct {
    const Self = @This();

    records: IdTypeMap,
    functions: IdTypeMap,
    arena: std.heap.ArenaAllocator, //stores typeinfo allocations for record fields
    state: *ir.IRState = undefined, //needs to know for comptime values

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .records = IdTypeMap.init(allocator),
            .functions = IdTypeMap.init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.records.deinit();
        self.functions.deinit();
        self.arena.deinit();
    }

    pub fn generate(self: *Self, dt: AST.DefinedType) !TypeInfo {
        return switch (dt) {
            .Basic => |typ| {
                if (Primitive.get(typ.tag.Identifier)) |info| return info;
                if (self.records.get(typ.tag.Identifier)) |info| return info;
                if (std.mem.eql(u8, typ.tag.Identifier, "string")) {
                    return try self.generate_wide_pointer(Primitive.get("byte").?);
                }

                std.log.err("Undeclared type {s}", .{typ});
                return IRError.Undeclared;
            },
            .Pointer => |p| blk: {
                var child = self.new_info();
                child.* = try self.generate(p.pointing_to);
                break :blk .{
                    .size = 8,
                    .tag = .Pointer,
                    .child = .{ .type_info = child },
                };
            },
            .Array => |arr| blk: {
                self.state.enter_ct_mode();
                defer self.state.exit_ct_mode();

                const t_info = try ir.ExpressionGenerator.generate_rvalue(self.state, arr.length);
                try types_equivalent(t_info, Primitive.get("int").?);
                const result = try self.state.execute_ct_buffer();
                const length = result[0];

                var child = self.new_info();
                child.* = try self.generate(arr.element_typ);
                break :blk .{
                    .size = child.size * length,
                    .tag = .Array,
                    .child = .{ .type_info = child },
                };
            },
            .WidePointer => |wp| try self.generate_wide_pointer(try self.generate(wp.pointing_to)),
        };
    }

    pub fn generate_wide_pointer(self: *Self, child_type_info: TypeInfo) !TypeInfo {
        const map = self.arena.allocator().create(FieldInfo) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
        map.* = FieldInfo.init(self.arena.allocator());
        const data_field = FieldInfoStruct{ .offset = 0, .type_info = child_type_info };
        const length_field = FieldInfoStruct{ .offset = 8, .type_info = Primitive.get("int").? };
        map.put("data_ptr", data_field) catch @panic("FATAL COMPILER ERROR: Out of memory");
        map.put("length", length_field) catch @panic("FATAL COMPILER ERROR: Out of memory");
        return .{
            .size = 16,
            .tag = .WidePointer,
            .child = .{ .field_info = map },
        };
    }

    pub fn register_record(self: *Self, decl: *AST.RecordDeclarationNode) !void {
        const map = self.arena.allocator().create(FieldInfo) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
        map.* = FieldInfo.init(self.arena.allocator());
        var record = TypeInfo{
            .size = 0,
            .child = .{ .field_info = map },
            .tag = .Record,
        };

        var cur_offset: usize = 0;

        var ast_field_o = decl.fields;
        while (ast_field_o) |ast_field| {
            switch (ast_field.typ) {
                .Basic => |tk| {
                    if (std.mem.eql(u8, tk.tag.Identifier, decl.name_tk.tag.Identifier)) {
                        std.log.err("Recursive data structure defined here {s}", .{tk});
                        return IRError.Duplicate;
                    }
                },
                else => {},
            }
            const field_ti = try self.generate(ast_field.typ);
            record.size += field_ti.size;

            const new_fieldinfo = FieldInfoStruct{
                .offset = cur_offset,
                .type_info = field_ti,
            };

            map.put(ast_field.name_tk.tag.Identifier, new_fieldinfo) catch {
                @panic("FATAL COMPILER ERROR: Out of memory");
            };

            cur_offset += field_ti.size;
            ast_field_o = ast_field.next;
        }

        self.records.put(decl.name_tk.tag.Identifier, record) catch {
            @panic("FATAL COMPILER ERROR: Out of memory!");
        };
    }

    pub fn register_function(self: *Self, decl: *AST.FunctionDeclarationNode) !void {
        if (self.functions.contains(decl.name_tk.tag.Identifier)) {
            std.log.err("Duplicate function definition {s}", .{decl.name_tk});
            return IRError.Duplicate;
        }
        const ret_type_info = try self.generate(decl.return_typ);
        self.functions.put(decl.name_tk.tag.Identifier, ret_type_info) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }

    pub fn new_info(self: *Self) *TypeInfo {
        return self.arena.allocator().create(TypeInfo) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };
    }
};

//more will be added in the future for now types are very strict
//TODO: better errors
pub fn types_equivalent(t1: TypeInfo, t2: TypeInfo) !void {
    errdefer {
        std.log.err("Type mismatch.", .{});
    }
    if (t1.tag == .Array and t2.tag == .Array) {
        try types_equivalent(t1.child.?.type_info.*, t2.child.?.type_info.*);
        if (t1.size / t1.child.?.type_info.*.size != t2.size / t2.child.?.type_info.*.size) return IRError.TypeMismatch;
    }
    if (t1.tag == .WidePointer and t2.tag == .WidePointer) {
        //eventually make sure that data_ptr field is pointing to the same type
        return;
    }
    if (t1.tag == t2.tag) return;
    if ((t1.tag == .Byte or t1.tag == .Integer or t1.tag == .Float) and t2.tag == .UntypedInt) return;
    if ((t2.tag == .Byte or t2.tag == .Integer or t2.tag == .Float) and t1.tag == .UntypedInt) return;

    return IRError.TypeMismatch;
}

pub const TypeInfo = struct {
    const Child = union {
        field_info: *FieldInfo,
        type_info: *TypeInfo,
    };

    size: usize,
    tag: TypeTag,
    child: ?Child,
};
