const std = @import("std");

const Instructions = @import("ir.zig").Instructions;

pub fn compile_ir(instructions: []Instructions, output_fd: std.fs.File) !void {
    var bw = std.io.bufferedWriter(output_fd.writer());
    const w = bw.writer();
    defer bw.flush() catch {};

    const registers = [_][]const u8{ "rax", "rsi", "rdi", "r8", "r9", "r10", "r11" };
    var rp: usize = 0;

    for (instructions) |inst| {
        switch (inst) {
            .LiteraLValue => |val| {
                try w.print("mov {s}, {d}\n", .{ registers[rp], val });
            },
            .StackAddr => |offset| {
                try w.print("mov {s}, rsp\n", .{registers[rp]});
                try w.print("add {s}, {d}\n", .{ registers[rp], offset });
            },
            .Load => {
                try w.print("mov {s}, [{s}]\n", .{ registers[rp], registers[rp] });
            },
            else => {},
        }
        next_reg(&rp);
    }
}

fn next_reg(rp: *usize) void {
    if (rp.* == 4) {
        rp.* = 0;
    }
    rp.* = rp.* + 1;
}
