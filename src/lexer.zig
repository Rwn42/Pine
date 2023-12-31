const std = @import("std");

const Location = @import("common.zig").Location;
const StringManager = @import("common.zig").StringManager;

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

//TODO: lex numbers of different bases / numbers like 1_000_000
//TODO: rigorous tests

const PredicateFn = fn (c: u8) bool;

fn is_identifier(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

fn is_comment_text(c: u8) bool {
    return !(c == '\n');
}

fn is_string_text(c: u8) bool {
    return !(c == '"' or c == '\n');
}

pub const Lexer = struct {
    const Self = @This();

    sm: *StringManager,
    loc: Location,
    contents: []u8,
    pos: usize,

    // Lexer does NOT own file content once all tokens are consumed file is safe to dealloc
    // all strings that are nessecary to save will be stored within the string manager
    pub fn init(contents: []u8, filepath: []const u8, sm: *StringManager) ?Self {
        if (contents.len < 0) {
            std.log.err("File {s} is empty", .{filepath});
            return null;
        }
        return .{
            .sm = sm,
            .loc = .{ .row = 1, .col = 1, .filename = filepath },
            .contents = contents,
            .pos = 0,
        };
    }

    // return type is optional because the error does not matter to the consumer of `next`
    // most code in this code base will print its own error messages
    pub fn next(self: *Self) ?Token {
        //skip leading whitespace also handles newlines
        while (std.ascii.isWhitespace(self.char())) {
            if (self.char() == '\n') {
                self.loc.row += 1;
                self.loc.col = 0;
            }
            self.adv();
        }

        var cwt = Token{
            .loc = self.loc,
            .tag = .EOF,
            .tagRepr = null,
        };

        const initial = self.char();

        cwt.tag = switch (initial) {
            0 => .EOF,
            ';' => .Semicolon,
            ':' => .Colon,
            '(' => .Lparen,
            ')' => .Rparen,
            '{' => .Lbrace,
            '}' => .Rbrace,
            '[' => .Lbracket,
            ']' => .Rbracket,
            '^' => .Hat,
            '&' => .Ampersand,
            ',' => .Comma,
            '.' => .Dot,
            '*' => .Asterisk,
            '+' => .Plus,
            '=' => self.lex_followed_by_equal(.DoubleEqual, .Equal),
            '>' => self.lex_followed_by_equal(.GreaterThanEqual, .GreaterThan),
            '<' => self.lex_followed_by_equal(.LessThanEqual, .LessThan),
            '!' => self.lex_followed_by_equal(.NotEqual, .ExclamationMark),
            '-' => blk: {
                const c = self.peek() orelse break :blk .Dash;
                if (std.ascii.isDigit(c)) break :blk self.lex_number() catch return null;
                break :blk .Dash;
            },
            '0'...'9' => self.lex_number() catch return null,
            'A'...'Z', 'a'...'z' => blk: {
                const start_idx = self.pos;
                self.adv_while(is_identifier);
                const text = self.contents[start_idx .. self.pos + 1];
                break :blk TokenType.Keywords.get(text) orelse .{ .Identifier = self.sm.alloc(text) };
            },
            '/' => blk: {
                const c = self.peek() orelse break :blk .SlashForward;
                if (c == '/') {
                    self.adv_while(is_comment_text);
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
                break :blk .{ .String = self.sm.alloc(self.contents[start_idx + 1 .. self.pos]) };
            },
            else => {
                std.log.err("Unexpected character {c} at {s}", .{ initial, self.loc });
                return null;
            },
        };

        cwt.tagRepr = TokenType.Repr.get(@tagName(cwt.tag));

        self.adv();

        return cwt;
    }

    // TODO: name this better
    // this function is for any token of the following format [something]= such as >= <= != +=,
    fn lex_followed_by_equal(self: *Self, true_case: TokenType, false_case: TokenType) TokenType {
        const next_char = self.peek() orelse return false_case;
        if (next_char == '=') {
            self.adv();
            return true_case;
        }
        return false_case;
    }

    fn lex_number(self: *Self) !TokenType {
        const start_idx = self.pos;
        self.adv_while(std.ascii.isDigit);
        const whole_component = self.contents[start_idx .. self.pos + 1];

        if (self.peek() != '.') {
            const n = std.fmt.parseInt(i64, whole_component, 10) catch |err| {
                std.log.err("Could not parse integer {s} {s}", .{ whole_component, self.loc });
                return err;
            };
            return .{ .Integer = n };
        }

        const whole_n = std.fmt.parseFloat(f64, whole_component) catch |err| {
            std.log.err("Could not parse float {s} {s}", .{ whole_component, self.loc });
            return err;
        };

        self.adv();
        const decimal_start = self.pos;
        self.adv_while(std.ascii.isDigit);

        const decimal_component = self.contents[decimal_start .. self.pos + 1];
        const decimal_n = std.fmt.parseFloat(f64, decimal_component) catch |err| {
            std.log.err("Could not parse float {s} {s}", .{ whole_component, self.loc });
            return err;
        };

        return .{ .Float = whole_n + decimal_n };
    }

    //advances to the character before the predicate function fails to return true
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
};
