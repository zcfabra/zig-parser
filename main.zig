const std = @import("std");

const ArrayList = std.ArrayList;
const allocator = std.heap.page_allocator;
const print = std.debug.print;

const TokenType = enum {
    LPAREN,
    BANG,
    EQ,
    NOT_EQ,
    RPAREN,
    NUM,
    ADD,
    MUL,
    SUB,
    DIV,
};

const Token = struct {
    literal: []const u8,
    token_type: TokenType,
    pub fn init(literal: []const u8, tt: TokenType) Token {
        return Token{
            .literal = literal,
            .token_type = tt,
        };
    }
};

pub fn get_tokens(src: []const u8) *[]Token {
    var list = ArrayList(u8).init(allocator);
    defer list.deinit();

    for (src) |char| {
        if (char != ' ') {
            try list.append(char);
        }
    }
    return [_]Token{};
}

const Tokenizer = struct {
    src: []const u8,
    r: usize,
    l: usize,
    fn init(src: []const u8) !Tokenizer {
        return Tokenizer{
            .src = try Tokenizer.remove_spaces(src),
            .l = 0,
            .r = 0,
        };
    }

    fn remove_spaces(src: []const u8) ![]const u8 {
        var new_str = ArrayList(u8).init(allocator);
        print("ALLOC STRING\n", .{});
        defer new_str.deinit();
        for (src) |ch| {
            if (ch != ' ') {
                try new_str.append(ch);
            }
        }
        return new_str.toOwnedSlice();
    }
    fn is_num(char: u8) bool {
        return ('0' <= char and char <= '9');
    }
    fn tokenize(self: *Tokenizer) ![]Token {
        var tokens = ArrayList(Token).init(allocator);
        defer tokens.deinit();

        while (self.r < self.src.len) {
            const char = self.src[self.r];
            switch (char) {
                '+' => {
                    const tok =
                        Token.init(
                        self.src[self.r .. self.r + 1],
                        TokenType.ADD,
                    );
                    self.r += 1;
                    self.l = self.r;
                    try tokens.append(tok);
                },
                '*' => {
                    const tok =
                        Token.init(
                        self.src[self.r .. self.r + 1],
                        TokenType.MUL,
                    );
                    self.r += 1;
                    self.l = self.r;
                    try tokens.append(tok);
                },
                '0'...'9' => {
                    while (self.r < self.src.len and Tokenizer.is_num(self.src[self.r])) {
                        self.r += 1;
                    }
                    const tok = Token.init(
                        self.src[self.l..self.r],
                        TokenType.MUL,
                    );
                    self.l = self.r;
                    try tokens.append(tok);
                },
                else => {},
            }
        }
        return tokens.toOwnedSlice();
    }
};

pub fn main() !void {
    const src = "10 + 90 + 10 * 90 + 10";
    var tokenizer = try Tokenizer.init(src);
    const tokens = try tokenizer.tokenize();
    for (tokens) |token| {
        print("{s}\n", .{token.literal});
    }
}
