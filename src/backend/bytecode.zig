const std = @import("std");

pub const Operation = struct {
    pub const Opcode = enum(u8) {
        load, //load a value from address which is at the top of the stack
        load8, //load a 8 byte value from address which is at the top of the stack
        store, //store a value from address which is at the top of the stack (value is second to the top)
        store8, //store a 8 byte value from address which is at the top of the stack (value is second to the top)
        astore, //store array of element size operand (stack: addr, length <- top)
        aload, //load array of element size operand
        push, //operand is 8 byte number which is the data to push
        add_i, //operand controls sign anything but 0 for subtraction
        add_f, //operand controls sign anything but 0 for subtraction
        mul_i, //operand controls sign anything but 0 for division
        mul_f, //operand controls sign anything but 0 for division
        lt_i, //operand controls sign anything but 0 for greater than
        lte_i, //operand controls sign anything but 0 for greater than equal
        lt_f, //operand controls sign anything but 0 for greater than
        lte_f, //operand controls sign anything but 0 for greater than equal
        eq, //operand controls sign anything but  0 for not equal is not type dependant just compares bytes
        not, //if value on stack is 1 sets it to 0 if its 0 sets it to 1 anything else is an error
        jmp, //jumps to operand position
        temp_print, //prints integer or character only a temporary instruction
        je, //jumps if to operand position if the second last value is equal
        call, //Operand is return address not call location call location is put on stack
        ret, //has no operand pops the call stack and goes to return address
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
