const std = @import("std");

// a location within a file used for error reporting
pub const Location = struct {
    row: usize,
    col: usize,
    filename: []const u8,

    pub fn format(self: Location, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}:{d}:{d}", .{ self.filename, self.row, self.col });
    }
};

// contains all of the strings we need to keep around for the duration of the program
// identifiers and string literals mostly
pub const StringManager = struct {
    arena: std.heap.ArenaAllocator,
    exists_table: std.StringHashMap([]u8),

    pub fn init(allocator: std.mem.Allocator) StringManager {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .exists_table = std.StringHashMap([]u8).init(allocator),
        };
    }

    pub fn alloc(s: *StringManager, string: []u8) []u8 {
        if (s.exists_table.get(string)) |ptr| return ptr; //string is already in the map

        var new = s.arena.allocator().dupe(u8, string) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };

        s.exists_table.put(string, new) catch {
            @panic("FATAL COMPILER ERROR: Out of memory");
        };

        return new;
    }

    pub fn destroy(s: *StringManager) void {
        s.exists_table.deinit();
        s.arena.deinit();
    }
};
