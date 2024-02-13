const std = @import("std");

const IR = @import("ir.zig").IR;
const IRInstruction = @import("ir.zig").IRInstruction;

pub fn fasm_compile(filename: []const u8, ir: IR) !void {
    const output_fd = std.fs.cwd().createFile(filename, .{}) catch {
        std.log.err("Output file {s} could not be created", .{filename});
        return;
    };
    defer output_fd.close();

    var bw = std.io.bufferedWriter(output_fd.writer());
    defer bw.flush() catch {};
    const w = bw.writer();

    //fasm file header
    _ = try w.write("format ELF64\n");

    try fasm_exec_preamble(w, ir.function_names, ir.externals);

    try fasm_data(w, ir.static);
}

fn fasm_data(writer: anytype, static: []u8) !void {
    _ = try writer.write("section '.data' writeable\n");
    _ = try writer.write("static_start: db 0\n");

    for (static, 0..) |b, i| {
        //every 16 bytes we want to make a new line to keep the asm file pretty
        if (i % 16 == 0) {
            _ = try writer.write("\n db");
        }
        if (i == static.len - 1) {
            try writer.print("{x}\n", .{b});
        } else {
            try writer.print("{x}, ", .{b});
        }
    }
}

fn fasm_exec_preamble(writer: anytype, functions: [][]const u8, externals: [][]const u8) !void {
    _ = try writer.write("section '.text' executable\n");

    for (functions) |function| {
        try writer.print("public {s}\n", .{function});
    }
    for (externals) |external| {
        try writer.print("extrn {s}\n", .{external});
    }
}

const RegisterSelector = struct {
    const Registers = [_][]u8{ "r8", "r9", "r10", "r11" };

    current: usize = 0,

    fn next(rs: *RegisterSelector) void {
        if (rs == Registers.len - 1) {
            rs = 0;
        } else {
            rs += 1;
        }
    }

    fn reg(rs: RegisterSelector) []u8 {
        return Registers[rs.current];
    }
};

fn fasm_exec(writer: anytype, instructions: []IRInstruction) !void {
    for (instructions) |inst| {
        switch (inst) {
            .Function => |name| {
                try writer.print("{s}:\n", .{name});
                //TODO: x86 preamble
            },
        }
    }
}
