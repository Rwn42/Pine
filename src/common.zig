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

//general purpose stack
pub fn Stack(comptime T: type, comptime limit: usize, comptime msg: []const u8) type {
    return struct {
        const Self = @This();
        const overflow_message = msg;

        sp: usize,
        buffer: [limit]T,

        pub fn init() Self {
            return .{
                .sp = 0,
                .buffer = undefined,
            };
        }

        //only push could happen user side an under flow or out of bounds access would be my fault
        pub fn push(self: *Self, value: T) void {
            if (self.sp >= limit - 1) {
                std.log.err("{s}", .{overflow_message});
                @panic("FATAL COMPILER ERROR: Stack Overflow");
            }
            self.buffer[self.sp] = value;
            self.sp += 1;
        }

        pub fn pop(self: *Self) void {
            if (self.sp <= 0) {
                @panic("FATAL COMPILER ERROR: Stack Underflow");
            }
            self.sp -= 1;
        }

        pub fn pop_ret(self: *Self) T {
            const a = self.top();
            if (self.sp <= 0) {
                @panic("FATAL COMPILER ERROR: Stack Underflow");
            }
            self.sp -= 1;
            return a;
        }

        pub fn top(self: *Self) T {
            return self.buffer[self.sp - 1];
        }

        pub fn top_idx(self: *Self) usize {
            return self.sp - 1;
        }

        pub fn get(self: *Self, idx: usize) T {
            if (idx < 0 or idx >= limit) @panic("Out of bounds access");
            return self.buffer[idx];
        }
    };
}
