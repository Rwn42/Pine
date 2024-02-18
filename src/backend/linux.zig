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
            _ = try writer.write("\n db ");
        }
        if (i == static.len - 1) {
            try writer.print("0x{x}\n", .{b});
        } else {
            try writer.print("0x{x}, ", .{b});
        }
    }
}

fn fasm_exec_preamble(writer: anytype, functions: [][]const u8, externals: [][]const u8) !void {
    _ = try writer.write("section '.text' executable\n");

    for (functions) |function| {
        try writer.print("public pine_{s}\n", .{function});
    }

    //TODO: this will break once importing files works
    for (externals) |external| {
        try writer.print("extrn {s}\n", .{external});
    }
}

const CcallRegisters = [_][]const u8{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

fn fasm_exec(writer: anytype, instructions: []IRInstruction) !void {
    for (instructions) |inst| {
        switch (inst) {
            .Function => |data| {
                try writer.print("pine_{s}:\n", .{data.name});
                //try writer.print("enter 0, 0\n", .{data.stack_size});
                try writer.print("enter 0, 0 \n", .{});
            },
            .ReturnAddr => {
                try writer.print("push rdi\n", .{});
            },
            .Call => |name| {
                try writer.print("call pine_{s}\n", .{name});
            },
            .CCall => |data| {
                //we use both rdi and rsi for our internal langugae so save before trampling
                try writer.print("mov r10, rdi\n", .{});
                try writer.print("mov r11, rsi\n", .{});
                for (0..data.param_n) |i| {
                    try writer.print("pop {s}\n", .{CcallRegisters[i]});
                }
                try writer.print("mov rax, 0\n", .{});
                try writer.print("mov al, 0\n", .{});
                try writer.print("enter 0, 0\n", .{});
                try writer.print("call {s}\n", .{data.name});
                try writer.print("leave\n", .{});
                try writer.print("mov rsi, r11 \n", .{});
                try writer.print("mov rdi, r10 \n", .{});
            },
            .Ret => {
                try writer.print("leave\n", .{});
                try writer.print("ret\n", .{});
            },
            .PushB => |val| {
                try writer.print("pushw {d}\n", .{val});
            },
            .PushW => |val| {
                try writer.print("pushq {d}\n", .{val});
            },
            .StackAddr => |offset| {
                try writer.print("mov rax, rbp\n", .{});

                //params have positive x86 stack offset but negative in the IR
                if (offset >= 0) {
                    try writer.print("sub rax, {d}\n", .{offset});
                } else {
                    try writer.print("add rax, {d}", .{offset * -1});
                }

                try writer.print("push rax\n", .{});
            },
            .StaticStart => |offset| {
                try writer.print("mov rax, static_start\n", .{});
                try writer.print("inc rax\n", .{});
                try writer.print("add rax, {d}\n", .{offset});
                try writer.print("pushq rax\n", .{});
            },
            .Add_I => |negate| {
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                if (negate) {
                    try writer.print("sub rbx, rax\n", .{});
                } else {
                    try writer.print("add rbx, rax\n", .{});
                }
                try writer.print("push rbx\n", .{});
            },
            .Mul_I => |negate| {
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                if (negate) {
                    try writer.print("div rbx, rax\n", .{});
                } else {
                    try writer.print("mul rbx, rax\n", .{});
                }
                try writer.print("push rbx\n", .{});
            },
            .StoreB => {
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                try writer.print("mov [rax], bx\n", .{});
                try writer.print("sub rsp, 2\n", .{});
            },
            .StoreW => {
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                try writer.print("mov QWORD [rax], rbx\n", .{});
                try writer.print("sub rsp, 8\n", .{});
            },
            .LoadB => {
                try writer.print("pop ax\n", .{});
                try writer.print("push WORD [rax]\n", .{});
            },
            .LoadW => {
                try writer.print("pop rax\n", .{});
                try writer.print("pushq [rax]\n", .{});
            },
            .TempStore => {
                try writer.print("pop rsi\n", .{});
            },
            .TempLoad => {
                try writer.print("push rsi\n", .{});
            },
            else => {},
        }
    }
}

//NOTE: this runtime is small, literally just calls main and exits
pub fn fasm_runtime() !void {
    const output_fd = std.fs.cwd().createFile("./out/pine_runtime.fasm", .{}) catch {
        std.log.err("Output file {s} could not be created", .{"pine runtime"});
        return;
    };
    defer output_fd.close();

    var bw = std.io.bufferedWriter(output_fd.writer());
    defer bw.flush() catch {};
    const w = bw.writer();

    _ = try w.write("format ELF64\n");
    _ = try w.write("section '.text' executable\n");
    _ = try w.write("extrn pine_main\n");
    _ = try w.write("public _start\n");
    _ = try w.write("_start:\n");
    _ = try w.write("call pine_main \n");
    _ = try w.write("mov rax, 60\n");
    _ = try w.write("mov rdi, 0\n");
    _ = try w.write("syscall\n");
}

fn fasm_size(size: usize) []const u8 {
    return @tagName(@as(FasmSize, @enumFromInt(size)));
}
