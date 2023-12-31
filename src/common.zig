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

    pub fn init(allocator: std.mem.Allocator) StringManager {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn alloc(s: *StringManager, string: []u8) []u8 {
        return s.arena.allocator().dupe(u8, string) catch {
            @panic("COMPILER ERROR: String manager allocation failed [out of memory].");
        };
    }

    pub fn destroy(s: *StringManager) void {
        s.arena.deinit();
    }
};
