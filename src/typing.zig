const std = @import("std");

const AST = @import("ast.zig");
const IRError = @import("ir.zig").IRError;

const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;

const IdTypeMap = std.StringHashMap(TypeInfo);

//TODO: Recursive Records

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

const FieldInfoStruct = struct {
    type_info: TypeInfo,
    offset: usize,
};

const FieldInfo = std.StringArrayHashMap(FieldInfoStruct);

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
                        return IRError.DuplicateDefinition;
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

            cur_offset += field_ti.size / 8;
            ast_field_o = ast_field.next;
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
        field_info: *FieldInfo,
        type_info: *TypeInfo,
    };

    size: usize,
    tag: TypeTag,
    child: ?Child,
};
