const std = @import("std");
const parser = @import("parser.zig");

pub const TokenType = enum {
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
    IDENTIFIER,
};

const TokenizerError = error{
    InvalidTokenType,
    MemoryError,
};

pub const Token = struct {
    literal: []const u8,
    type: TokenType,
    pub fn init(literal: []const u8, token_type: TokenType) Token {
        return Token{
            .literal = literal,
            .type = token_type,
        };
    }

    pub fn get_precedence(self: *const Token) parser.Precendence {
        return switch (self.type) {
            TokenType.MUL, TokenType.DIV => parser.Precendence.DIVMUL,
            TokenType.ADD, TokenType.SUB => parser.Precendence.ADDSUB,
            else => parser.Precendence.LOWEST,
        };
    }
    pub fn is_unary_operator(self: *const Token) bool {
        return switch (self.type) {
            TokenType.BANG => true,
            else => false,
        };
    }
    pub fn is_binary_operator(self: *const Token) bool {
        return switch (self.type) {
            TokenType.ADD, TokenType.MUL, TokenType.EQ, TokenType.SUB, TokenType.DIV => true,
            else => false,
        };
    }
};

pub const Tokenizer = struct {
    src: []const u8,
    r: usize,
    l: usize,
    pub fn init(src: []const u8, allocator: *std.mem.Allocator) !Tokenizer {
        return Tokenizer{
            .src = try Tokenizer.remove_spaces(src, allocator),
            .l = 0,
            .r = 0,
        };
    }

    fn remove_spaces(src: []const u8, allocator: *std.mem.Allocator) ![]const u8 {
        var new_str = std.ArrayList(u8).init(allocator.*);
        defer new_str.deinit();
        for (src) |ch| {
            if (ch != ' ') {
                try new_str.append(ch);
            }
        }
        return new_str.toOwnedSlice();
    }
    fn is_alpha(char: u8) bool {
        return ('A' <= char and char <= 'Z') or ('a' <= char and char <= 'z');
    }
    fn is_num(char: u8) bool {
        return ('0' <= char and char <= '9');
    }
    fn get_char_token(
        self: *Tokenizer,
        token_type: TokenType,
    ) Token {
        const token =
            Token.init(
            self.src[self.r .. self.r + 1],
            token_type,
        );
        self.r += 1;
        self.l = self.r;
        return token;
    }
    pub fn tokenize(self: *Tokenizer, allocator: *std.mem.Allocator) TokenizerError![]Token {
        var tokens = std.ArrayList(Token).init(allocator.*);
        defer tokens.deinit();
        var tok: ?Token = null;

        while (self.r < self.src.len) {
            const char = self.src[self.r];
            switch (char) {
                '!' => {
                    tok = self.get_char_token(TokenType.BANG);
                },
                '+' => {
                    tok = self.get_char_token(TokenType.ADD);
                },
                '-' => {
                    tok = self.get_char_token(TokenType.SUB);
                },
                '*' => {
                    tok = self.get_char_token(TokenType.MUL);
                },
                '/' => {
                    tok = self.get_char_token(TokenType.DIV);
                },
                '(' => {
                    tok = self.get_char_token(TokenType.LPAREN);
                },
                ')' => {
                    tok = self.get_char_token(TokenType.RPAREN);
                },
                '0'...'9' => {
                    while (self.r < self.src.len and Tokenizer.is_num(self.src[self.r])) {
                        self.r += 1;
                    }
                    tok = Token.init(
                        self.src[self.l..self.r],
                        TokenType.NUM,
                    );
                    self.l = self.r;
                },
                'A'...'Z', 'a'...'z' => {
                    while (self.r < self.src.len and Tokenizer.is_alpha(self.src[self.r])) {
                        self.r += 1;
                    }
                    tok = Token.init(
                        self.src[self.l..self.r],
                        TokenType.IDENTIFIER,
                    );
                    self.l = self.r;
                },
                else => {
                    return TokenizerError.InvalidTokenType;
                },
            }
            tokens.append(tok.?) catch {
                return TokenizerError.InvalidTokenType;
            };
        }
        return tokens.toOwnedSlice() catch {
            return TokenizerError.MemoryError;
        };
    }
};
