const std = @import("std");

const Operation = @import("bytecode.zig").Operation;
const Stack = @import("../common.zig").Stack;

const OperandStack = Stack(u64, 1024, "Expression is too long");
const CallStack = Stack(*StackFrame, 128, "Recursion Depth Reached");

//TODO: Cleanup
//TODO: look for simple optimizations

pub const InterpreterError = error{
    Done,
    UnexpectedEnd,
};

pub const StackFrame = struct {
    locals: [1024]u8,
    return_addr: usize,
};

pub const Interpreter = struct {
    program: []Operation,
    operand_stack: OperandStack,
    call_stack: CallStack,
    locals: *[1024]u8,
    ip: usize = 0,
    allocator: std.mem.Allocator,
    temp_r: u64 = 0,

    pub fn init_from_program(program: []Operation, allocator: std.mem.Allocator) Interpreter {
        return .{
            .program = program,
            .operand_stack = OperandStack.init(),
            .call_stack = CallStack.init(),
            .allocator = allocator,
            .locals = undefined,
        };
    }

    pub fn run(i: *Interpreter) !void {
        while (true) {
            run_inst(i) catch |e| {
                switch (e) {
                    InterpreterError.Done => {},
                    else => std.log.err("{any}", .{e}),
                }
                return;
            };
        }
    }

    fn print(i: *Interpreter) void {
        if (i.call_stack.sp > 0) {
            std.debug.print("\n \n \nLocals (0-31): ", .{});
            for (i.locals.*[0..128]) |elem| {
                std.debug.print("{d} ", .{elem});
            }
        }

        std.debug.print("\nOperand Stack: ", .{});
        for (i.operand_stack.buffer[0..i.operand_stack.sp]) |elem| {
            std.debug.print("{d} ", .{elem});
        }
    }

    pub fn run_inst(i: *Interpreter) !void {
        if (i.ip >= i.program.len) {
            return InterpreterError.UnexpectedEnd;
        }
        const inst = i.program[i.ip];
        //print(i);
        switch (inst.opc) {
            .push => {
                i.operand_stack.push(inst.operand.?);
            },
            .mul_i => {
                const b: i64 = @bitCast(i.operand_stack.pop_ret());
                const a: i64 = @bitCast(i.operand_stack.pop_ret());
                if (inst.operand.? == 1) {
                    i.operand_stack.push(@bitCast((@divTrunc(a, b))));
                } else {
                    i.operand_stack.push(@bitCast((a * b)));
                }
            },
            .add_i => {
                const b: i64 = @bitCast(i.operand_stack.pop_ret());
                const a: i64 = @bitCast(i.operand_stack.pop_ret());
                if (inst.operand.? == 1) {
                    i.operand_stack.push(@bitCast((a - b)));
                } else {
                    i.operand_stack.push(@bitCast((a + b)));
                }
            },

            .eq => {
                const b = i.operand_stack.pop_ret();
                const a = i.operand_stack.pop_ret();
                if (inst.operand.? == 0) {
                    i.operand_stack.push(if (a == b) 1 else 0);
                } else {
                    i.operand_stack.push(if (a == b) 0 else 1);
                }
            },

            .lt_i => {
                const b: i64 = @bitCast(i.operand_stack.pop_ret());
                const a: i64 = @bitCast(i.operand_stack.pop_ret());
                if (inst.operand.? == 0) {
                    i.operand_stack.push(if (a < b) 1 else 0);
                } else {
                    i.operand_stack.push(if (a > b) 1 else 0);
                }
            },

            .lte_i => {
                const b: i64 = @bitCast(i.operand_stack.pop_ret());
                const a: i64 = @bitCast(i.operand_stack.pop_ret());
                if (inst.operand.? == 0) {
                    i.operand_stack.push(if (a <= b) 1 else 0);
                } else {
                    i.operand_stack.push(if (a >= b) 1 else 0);
                }
            },

            .jmp => {
                const loc = i.operand_stack.pop_ret();
                i.ip = loc;
                return;
            },
            .je => {
                const loc = i.operand_stack.pop_ret();
                const val = i.operand_stack.pop_ret();
                if (inst.operand.? == 0) {
                    if (val == 1) {
                        i.ip = loc;
                        return;
                    }
                } else {
                    if (val != 1) {
                        i.ip = loc;
                        return;
                    }
                }
            },

            .temp_print => {
                switch (inst.operand.?) {
                    0 => std.debug.print("{d} \n", .{i.operand_stack.pop_ret()}),
                    1 => std.debug.print("{c}", .{@as(u8, @intCast(i.operand_stack.pop_ret()))}),
                    else => {},
                }
            },

            .not => {
                const a = i.operand_stack.pop_ret();
                if (a != 1 and a != 0) @panic("Cannot negate a non boolean");
                i.operand_stack.push(if (a == 1) 0 else 1);
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
                i.locals.*[position] = @as(u8, @intCast(value));
            },
            .store8 => {
                const position = i.operand_stack.pop_ret();
                const value = @as([8]u8, @bitCast(i.operand_stack.pop_ret()));
                for (value, 0..) |byte, idx| {
                    i.locals.*[position + idx] = byte;
                }
            },
            .load => {
                const position = i.operand_stack.pop_ret();
                i.operand_stack.push(i.locals.*[position]);
            },
            .load8 => {
                const position = i.operand_stack.pop_ret();
                var bytes: [8]u8 = [_]u8{0} ** 8;
                std.mem.copy(u8, &bytes, i.locals.*[position .. position + 8]);
                const value = std.mem.bytesAsValue(u64, &bytes).*;
                i.operand_stack.push(value);
            },
            .call => {
                const stackFrame = i.allocator.create(StackFrame) catch {
                    @panic("Out of memory!");
                };
                stackFrame.* = StackFrame{ .locals = [_]u8{0} ** 1024, .return_addr = inst.operand orelse 0 };
                i.locals = &(stackFrame.locals);
                const position = i.operand_stack.pop_ret();
                i.call_stack.push(stackFrame);
                i.ip = position;
                return;
            },
            .ret => {
                var frame = i.call_stack.pop_ret();
                defer i.allocator.destroy(frame);
                i.ip = frame.return_addr;
                if (i.ip == 0) return InterpreterError.Done;
                return;
            },

            else => @panic("Not implemented"),
        }
        i.ip += 1;
    }
};
