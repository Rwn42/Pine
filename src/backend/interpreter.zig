const std = @import("std");

const Operation = @import("bytecode.zig").Operation;
const Stack = @import("../common.zig").Stack;

const OperandStack = Stack(u64, 128, "Expression is too long");

pub const InterpreterError = error{
    Done,
    SomethingElse,
};

pub const Interpreter = struct {
    program: []Operation,
    operand_stack: OperandStack,
    locals: [1024]u8,
    ip: usize = 0,
    temp_r: u64 = 0,

    pub fn init_from_program(program: []Operation) Interpreter {
        return .{
            .program = program,
            .operand_stack = OperandStack.init(),
            .locals = [_]u8{0} ** 1024,
        };
    }

    pub fn run(i: *Interpreter) !void {
        defer {
            std.debug.print("\n \n \nLocals (0-31): ", .{});
            for (i.locals[0..31]) |elem| {
                std.debug.print("{d} ", .{elem});
            }

            std.debug.print("\nOperand Stack: ", .{});
            for (i.operand_stack.buffer[0..i.operand_stack.sp]) |elem| {
                std.debug.print("{d} ", .{elem});
            }
        }
        while (true) {
            run_inst(i) catch |e| {
                switch (e) {
                    InterpreterError.Done => return,
                    else => std.log.err("{any}", .{e}),
                }
            };
        }
    }

    pub fn run_inst(i: *Interpreter) !void {
        if (i.ip >= i.program.len) {
            return InterpreterError.Done;
        }
        const inst = i.program[i.ip];

        std.debug.print("\nOperand Stack: ", .{});
        for (i.operand_stack.buffer[0..i.operand_stack.sp]) |elem| {
            std.debug.print("{d} ", .{elem});
        }

        std.debug.print("\nLocals (0-31): ", .{});
        for (i.locals[0..31]) |elem| {
            std.debug.print("{d} ", .{elem});
        }

        std.debug.print("\ntemp: {d}", .{i.temp_r});

        switch (inst.opc) {
            .push => {
                i.operand_stack.push(inst.operand.?);
            },
            .mul_i => {
                const a: i64 = @bitCast(i.operand_stack.pop_ret());
                const b: i64 = @bitCast(i.operand_stack.pop_ret());
                i.operand_stack.push(@bitCast((a * b)));
            },
            .add_i => {
                const a: i64 = @bitCast(i.operand_stack.pop_ret());
                const b: i64 = @bitCast(i.operand_stack.pop_ret());
                i.operand_stack.push(@bitCast((a + b)));
            },
            .gstore => {
                i.temp_r = i.operand_stack.pop_ret();
            },
            .gload => {
                i.operand_stack.push(i.temp_r);
            },
            .store => {
                const position = i.operand_stack.pop_ret();
                const value = i.operand_stack.pop_ret();
                //std.debug.print("storing: {d} at {d} \n", .{ value, position });
                i.locals[position] = @as(u8, @intCast(value));
            },
            .store8 => {
                const position = i.operand_stack.pop_ret();
                const value = @as([8]u8, @bitCast(i.operand_stack.pop_ret()));
                for (value, 0..) |byte, idx| {
                    i.locals[position + idx] = byte;
                }
            },
            else => @panic("Not implemented"),
        }
        i.ip += 1;
    }
};
