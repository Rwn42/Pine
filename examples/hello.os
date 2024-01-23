String :: record {
    buffer: [16]byte,
    length: int,
}

print_string :: fn(s: String) void {
    i: int = 0;
    while i < s.length {
        #print s.buffer.i;
        i = i + 1;
    }
}

main :: fn() void {
    buf: [16]byte = 0;
    buf.0 = 72;
    buf.1 = 101;
    buf.2 = 108;
    buf.3 = 108;
    buf.4 = 111;
    buf.5 = 32;
    buf.6 = 87;
    buf.7 = 111;
    buf.8 = 114;
    buf.9 = 108;
    buf.10 = 100;
    buf.11 = 10;
    print_string(String{buffer: buf, length: 12});
}

