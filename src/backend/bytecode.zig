const std = @import("std");

//TODO: instruction documentation

pub const Operation = struct {
    pub const Opcode = enum(u8) {
        load,
        load8,
        store,
        store8,
        gload, //load value from general purpose register for now just an address
        gstore, //store value into general purpose register
        push, //NOTE: operand is 8 byte number which is the data to push
        add_i,
        add_f,
        mul_i,
        mul_f,
        lt_i,
        lte_i,
        lt_f,
        lte_f,
        eq,
        not,
        jmp,
        temp_print,
        je,
        call, //Operand is return address not call location
        ret,
    };

    opc: Opcode,
    operand: ?u64, // the signedness of the integer does not matter just 8 bytes

    pub fn format(self: Operation, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} ", .{@tagName(self.opc)});
        if (self.operand) |operand| try writer.print("{d}", .{operand});
    }
};
