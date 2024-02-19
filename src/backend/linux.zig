const std = @import("std");

const IR = @import("ir.zig").FileIR;
const IRInstruction = @import("ir.zig").IRInstruction;

//TODO: fix load/store for bytes

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

    try fasm_exec_preamble(w, ir.public, ir.external, ir.imported);
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
            if ((i + 1) % 16 == 0) {
                try writer.print("0x{x}", .{b});
            } else {
                try writer.print("0x{x}, ", .{b});
            }
        }
    }
}

fn fasm_exec_preamble(writer: anytype, functions: [][]const u8, externals: [][]const u8, imported: [][]const u8) !void {
    _ = try writer.write("section '.text' executable\n");

    for (functions) |function| {
        try writer.print("public pine_{s}\n", .{function});
    }

    for (externals) |external| {
        try writer.print("extrn {s}\n", .{external});
    }

    for (imported) |import| {
        try writer.print("extrn pine_{s}\n", .{import});
    }
}

const CcallRegisters = [_][]const u8{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

fn fasm_exec(writer: anytype, instructions: []IRInstruction) !void {
    for (instructions) |inst| {
        switch (inst) {
            .Function => |data| {
                _ = try writer.write(";;function decl\n");
                try writer.print("pine_{s}:\n", .{data.name});
                try writer.print("push rbp \n", .{});
                try writer.print("mov rbp, rsp \n", .{});
                try writer.print("sub rsp, {d}\n", .{data.stack_size});
                _ = try writer.write(";; end function decl\n\n");
            },
            .ReturnAddr => |size| {
                _ = try writer.write(";;generating return address\n");
                try writer.print("mov rax, rbp\n", .{});
                //16 puts you over ebp and return address
                try writer.print("add rax, {d}\n", .{size + 8});
                try writer.print("push rax\n", .{});
                _ = try writer.write(";;end generating return address\n\n");
            },
            .Reserve => |n| {
                _ = try writer.write(";;reserve for return sapce\n");
                try writer.print("sub rsp, {d}\n", .{n});
                _ = try writer.write(";;end reserve for return sapce\n\n");
            },
            .Call => |call| {
                _ = try writer.write(";;pine call\n");
                try writer.print("call pine_{s}\n", .{call.name});
                try writer.print("add rsp, {d}\n", .{call.param_size});
                _ = try writer.write(";;end pine call\n\n");
            },
            .CCall => |data| {
                _ = try writer.write(";;c call\n");

                //we use both rdi and rsi for our internal langugae so save before trampling
                try writer.print("mov r11, rsp\n", .{});
                for (0..data.param_n) |i| {
                    try writer.print("pop {s}\n", .{CcallRegisters[i]});
                }
                try writer.print("xor rax, rax\n", .{});
                try writer.print("and rsp, 0xFFFFFFFFFFFFFFF0\n", .{}); //allign stack
                try writer.print("call {s}\n", .{data.name});
                try writer.print("mov rsp, r11 \n", .{});

                _ = try writer.write(";;end c call\n\n");
            },
            .Ret => {
                _ = try writer.write(";;return\n");

                try writer.print("leave\n", .{});
                try writer.print("ret\n", .{});

                _ = try writer.write(";;end return\n\n");
            },
            .PushB => |val| {
                _ = try writer.write(";;push a byte\n");
                try writer.print("pushw {d}\n", .{val});
                _ = try writer.write(";;end push a byte\n\n");
            },
            .PushW => |val| {
                _ = try writer.write(";;push a word\n");
                try writer.print("pushq {d}\n", .{val});
                _ = try writer.write(";;end push a word\n\n");
            },
            .StackAddr => |offset| {
                _ = try writer.write(";;generating stack address\n");

                try writer.print("mov rax, rbp\n", .{});

                //params have positive x86 stack offset but negative in the IR
                if (offset >= 0) {
                    try writer.print("sub rax, {d}\n", .{offset});
                } else {
                    //plus  skips the return addr pointer and ebp
                    try writer.print("add rax, {d}\n", .{(offset * -1) + 8});
                }

                try writer.print("push rax\n", .{});
                _ = try writer.write(";;end generating stack address\n\n");
            },
            .StaticStart => |offset| {
                _ = try writer.write(";;generating static address\n");
                try writer.print("mov rax, static_start\n", .{});
                try writer.print("inc rax\n", .{});
                try writer.print("add rax, {d}\n", .{offset});
                try writer.print("pushq rax\n", .{});
                _ = try writer.write(";;end generating static address\n");
            },
            .Add_I => |negate| {
                _ = try writer.write(";;add/sub \n");
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                if (negate) {
                    try writer.print("sub rbx, rax\n", .{});
                } else {
                    try writer.print("add rbx, rax\n", .{});
                }
                try writer.print("push rbx\n", .{});
                _ = try writer.write(";;end add/sub \n\n");
            },
            .Mul_I => |negate| {
                _ = try writer.write(";;mul/divide \n");

                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                if (negate) {
                    try writer.print("div rbx, rax\n", .{});
                } else {
                    try writer.print("mul rbx, rax\n", .{});
                }
                try writer.print("push rbx\n", .{});
                _ = try writer.write(";;end mul/divide \n\n");
            },
            .StoreB => {
                _ = try writer.write(";;store byte\n");
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                try writer.print("mov [rax], bx\n", .{});
                _ = try writer.write(";;end store byte\n\n");
            },
            .StoreW => {
                _ = try writer.write(";;store word\n");
                try writer.print("pop rax\n", .{});
                try writer.print("pop rbx\n", .{});
                try writer.print("mov QWORD [rax], rbx\n", .{});
                _ = try writer.write(";;end store word\n\n");
            },
            .LoadB => {
                _ = try writer.write(";;load byte\n");
                try writer.print("pop ax\n", .{});
                try writer.print("push WORD [rax]\n", .{});
                _ = try writer.write(";;end load byte\n\n");
            },
            .LoadW => {
                _ = try writer.write(";;load word\n");
                try writer.print("pop rax\n", .{});
                try writer.print("pushq [rax]\n", .{});
                _ = try writer.write(";;end load word\n\n");
            },
            .TempStore => {
                _ = try writer.write(";;temp store\n");
                try writer.print("pop r10\n", .{});
                _ = try writer.write(";;end temp store\n\n");
            },
            .TempLoad => {
                _ = try writer.write(";;temp load\n");
                try writer.print("push r10\n", .{});
                _ = try writer.write(";;end temp load\n\n");
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
