const std = @import("std");

const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const FileLocation = @import("token.zig").FileLocation;

const PredicateFn = fn (c: u8) bool;

pub const Lexer = struct {
    const Self = @This();
    const StringManager = struct {
        arena: std.heap.ArenaAllocator,
        exists: std.StringHashMap(void),

        fn add(sm: *StringManager, s: []u8) []const u8 {
            const res = sm.exists.getOrPut(s) catch @panic("Out of memory!");
            if (res.found_existing) return res.key_ptr.*;
            const new = sm.arena.allocator().dupe(u8, s) catch @panic("Out of memory!");
            res.key_ptr.* = new;
            return new;
        }

        fn deinit(sm: *StringManager) void {
            sm.exists.deinit();
            sm.arena.deinit();
        }
    };

    sm: StringManager,
    loc: FileLocation,
    contents: []u8,
    pos: usize = 0,

    pub fn init(contents: []u8, filename: []const u8, allocator: std.mem.Allocator) Self {
        return .{
            .sm = StringManager{
                .arena = std.heap.ArenaAllocator.init(allocator),
                .exists = std.StringHashMap(void).init(allocator),
            },
            .loc = FileLocation{ .row = 1, .col = 1, .filename = filename },
            .contents = contents,
        };
    }

    pub fn next(self: *Self) ?Token {
        while (std.ascii.isWhitespace(self.char())) {
            if (self.char() == '\n') {
                var tk = Token{ .location = self.loc, .tag = .Newline };
                self.loc.row += 1;
                self.loc.col = 0;
                self.adv();
                return tk;
            }
            self.adv();
        }

        var cwt = Token{ .location = self.loc, .tag = .EOF };

        const initial = self.char();

        cwt.tag = switch (initial) {
            0 => .EOF,
            '(' => .Lparen,
            ')' => .Rparen,
            '{' => .Lbrace,
            '}' => .Rbrace,
            '[' => .Lbracket,
            ']' => .Rbracket,
            '^' => .Hat,
            '&' => .Ampersand,
            ',' => .Comma,
            '*' => .Asterisk,
            '+' => .Plus,
            '=' => self.lex_complex_operator(.DoubleEqual, .Equal),
            '>' => self.lex_complex_operator(.GreaterThanEqual, .GreaterThan),
            '<' => self.lex_complex_operator(.LessThanEqual, .LessThan),
            '!' => self.lex_complex_operator(.NotEqual, .ExclamationMark),
            '-' => .Dash,
            '.' => blk: {
                if (self.peek() != '.') break :blk .Dot;
                self.adv();
                break :blk .DoubleDot;
            },
            ':' => blk: {
                if (self.peek() != ':') break :blk .Colon;
                self.adv();
                break :blk .DoubleColon;
            },
            '1'...'9' => self.lex_number(10) catch return null,
            'A'...'Z', 'a'...'z' => blk: {
                const start_idx = self.pos;
                self.adv_while(is_num_or_ident);
                var text = self.contents[start_idx .. self.pos + 1];
                break :blk TokenTag.Keywords.get(text) orelse .{ .Identifier = self.sm.add(text) };
            },
            '#' => blk: {
                const start_idx = self.pos;
                self.adv();
                self.adv_while(is_num_or_ident);
                var text = self.contents[start_idx .. self.pos + 1];
                break :blk TokenTag.Keywords.get(text) orelse {
                    std.log.err("Directive {s} does not exist at {s}", .{ text, cwt.location });
                    return null;
                };
            },
            '0' => blk: {
                const c = self.peek() orelse break :blk .{ .Integer = 0 };
                if (std.ascii.isDigit(c)) break :blk self.lex_number(10) catch return null;
                break :blk switch (c) {
                    'x' => blk_inner: {
                        self.adv();
                        self.adv();
                        break :blk_inner self.lex_number(16) catch return null;
                    },
                    'b' => blk_inner: {
                        self.adv();
                        self.adv();
                        break :blk_inner self.lex_number(2) catch return null;
                    },
                    'o' => blk_inner: {
                        self.adv();
                        self.adv();
                        break :blk_inner self.lex_number(8) catch return null;
                    },
                    else => .{ .Integer = 0 },
                };
            },

            '/' => blk: {
                const c = self.peek() orelse break :blk .SlashForward;
                if (c == '/') {
                    self.adv_while(is_comment_text);
                    self.adv();
                    return self.next();
                }
                break :blk .SlashForward;
            },
            '"' => blk: {
                const start_idx = self.pos;
                self.adv_while(is_string_text);
                if (self.peek() != '"') {
                    std.log.err("No matching quotation for string ending at {s}", .{self.loc});
                    return null;
                }
                self.adv();
                break :blk .{ .String = self.sm.add(self.contents[start_idx + 1 .. self.pos]) };
            },
            else => {
                std.log.err("Unexpected character {c} at {s}", .{ initial, self.loc });
                return null;
            },
        };

        self.adv();

        return cwt;
    }

    fn lex_number(self: *Self, base: u8) !TokenTag {
        const start_idx = self.pos;
        self.adv_while(is_num_or_ident);
        const whole_component = self.contents[start_idx .. self.pos + 1];

        if (self.peek() != '.' or (self.peek() == '.' and self.peek2() == '.')) {
            const n = std.fmt.parseInt(i64, whole_component, base) catch |err| {
                std.log.err("Could not parse integer {s} {s}", .{ whole_component, self.loc });
                return err;
            };
            return .{ .Integer = n };
        }

        self.adv();

        if (base != 10) {
            std.log.err("Floating point number can only be base 10 {s} {s}", .{ whole_component, self.loc });
            return std.fmt.ParseFloatError.InvalidCharacter;
        }

        const whole_n = std.fmt.parseFloat(f64, whole_component) catch |err| {
            std.log.err("Could not parse float {s} {s}", .{ whole_component, self.loc });
            return err;
        };

        const decimal_start = self.pos;
        self.adv_while(std.ascii.isDigit);

        const decimal_component = self.contents[decimal_start .. self.pos + 1];
        const decimal_n = std.fmt.parseFloat(f64, decimal_component) catch |err| {
            std.log.err("Could not parse float {s} {s}", .{ decimal_component, self.loc });
            return err;
        };

        return .{ .Float = whole_n + decimal_n };
    }

    fn lex_complex_operator(self: *Self, true_case: TokenTag, false_case: TokenTag) TokenTag {
        const next_char = self.peek() orelse return false_case;
        if (next_char == '=') {
            self.adv();
            return true_case;
        }
        return false_case;
    }

    fn adv_while(self: *Self, comptime pred: PredicateFn) void {
        while (pred(self.peek() orelse return)) self.adv();
    }

    fn adv(self: *Self) void {
        self.pos += 1;
        self.loc.col += 1;
    }

    fn char(self: Self) u8 {
        if (self.pos > self.contents.len - 1) return 0;
        return self.contents[self.pos];
    }

    fn peek(self: Self) ?u8 {
        if (self.pos + 1 > self.contents.len - 1) return null;
        return self.contents[self.pos + 1];
    }

    fn peek2(self: Self) ?u8 {
        if (self.pos + 2 > self.contents.len - 1) return null;
        return self.contents[self.pos + 2];
    }

    fn is_num_or_ident(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    fn is_comment_text(c: u8) bool {
        return !(c == '\n');
    }

    fn is_string_text(c: u8) bool {
        return !(c == '"' or c == '\n');
    }
};
