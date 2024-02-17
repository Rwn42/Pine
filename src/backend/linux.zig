const std = @import("std");

const IR = @import("ir.zig").FileIR;
const IRInstruction = @import("ir.zig").IRInstruction;

const FasmSize = enum(u8) {
    QWORD = 8,
    BYTE = 1,
};

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

    try fasm_exec_preamble(w, ir.public, ir.external);
    try fasm_exec(w, ir.instructions);
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
        try writer.print("public pine_{s}\n", .{function});
    }
    for (externals) |external| {
        try writer.print("extrn pine_{s}\n", .{external});
    }
}

const RegisterSelector = struct {
    const Registers = [_][]const u8{ "r8", "r9", "r10", "r11" };

    current: usize = 0,

    fn next(rs: *RegisterSelector) void {
        if (rs.current == Registers.len - 1) {
            rs.current = 0;
        } else {
            rs.current += 1;
        }
    }

    fn prev(rs: *RegisterSelector) []const u8 {
        if (rs.current == 0) {
            return Registers[Registers.len - 1];
        } else {
            return Registers[rs.current - 1];
        }
    }

    fn reg(rs: RegisterSelector) []const u8 {
        return Registers[rs.current];
    }
};

fn fasm_exec(writer: anytype, instructions: []IRInstruction) !void {
    var rs = RegisterSelector{};
    _ = rs;
    for (instructions) |inst| {
        switch (inst) {
            .Function => |data| {
                try writer.print("pine_{s}:\n", .{data.name});
                try writer.print("enter {d}, 0\n", .{data.stack_size});
            },
            .Ret => {
                try writer.print("leave\n", .{});
                try writer.print("ret\n", .{});
            },
            else => {},
        }
    }
}

fn fasm_size(size: usize) []const u8 {
    return @tagName(@as(FasmSize, @enumFromInt(size)));
}
