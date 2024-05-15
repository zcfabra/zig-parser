const std = @import("std");

const ArrayList = std.ArrayList;
const print = std.debug.print;

fn createInit(alloc: std.mem.Allocator, comptime T: type, props: anytype) !*T {
    const new = try alloc.create(T);
    new.* = props;
    return new;
}

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
    IDENTIFIER,
};

pub const Precendence = enum(i32) {
    LOWEST,
    ADDSUB,
    DIVMUL,
    BANG,
};

const TokenizerError = error{
    InvalidTokenType,
    MemoryError,
};

const ParserError = error{
    MemoryError,
};

const Token = struct {
    literal: []const u8,
    type: TokenType,
    pub fn init(literal: []const u8, token_type: TokenType) Token {
        return Token{
            .literal = literal,
            .type = token_type,
        };
    }

    pub fn get_precedence(self: *const Token) Precendence {
        return switch (self.type) {
            TokenType.MUL, TokenType.DIV => Precendence.DIVMUL,
            TokenType.ADD, TokenType.SUB => Precendence.ADDSUB,
            else => Precendence.LOWEST,
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
        var new_str = ArrayList(u8).init(allocator.*);
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
        var tokens = ArrayList(Token).init(allocator.*);
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

const LiteralNode = struct {
    literal: []const u8,

    fn repr(self: *LiteralNode) []const u8 {
        return self.literal;
    }
};
const BinaryNode = struct {
    l: *AstNode,
    r: *AstNode,
    op: Token,

    fn repr(self: *BinaryNode, allocator: *std.mem.Allocator) ParserError![]const u8 {
        const l = try self.l.repr(allocator);
        const r = try self.r.repr(allocator);
        // defer allocator.free(l);
        // defer allocator.free(r);
        return std.fmt.allocPrint(
            allocator.*,
            "( {s} {s} {s} )",
            .{ l, self.op.literal, r },
        ) catch
            ParserError.MemoryError;
    }
};

const UnaryNode = struct {
    r: *AstNode,
    op: Token,

    fn repr(self: *UnaryNode, allocator: *std.mem.Allocator) ParserError![]const u8 {
        const r = try self.r.repr(allocator);
        // defer allocator.free(r);
        const str = std.fmt.allocPrint(
            allocator.*,
            "( {s} {s} )",
            .{ self.op.literal, r },
        ) catch
            return ParserError.MemoryError;
        return str;
    }
};

pub const AstNode = union(enum) {
    AstLiteralNode: *LiteralNode,
    AstBinaryNode: *BinaryNode,
    AstUnaryNode: *UnaryNode,

    pub fn repr(self: AstNode, allocator: *std.mem.Allocator) ParserError![]const u8 {
        return switch (self) {
            AstNode.AstLiteralNode => |literal| literal.repr(),
            AstNode.AstUnaryNode => |unary| try unary.repr(allocator),
            AstNode.AstBinaryNode => |binary| try binary.repr(allocator),
        };
    }
    fn init_binary(
        alloc: *std.mem.Allocator,
        op_token: Token,
        l: *AstNode,
        r: *AstNode,
    ) ParserError!*AstNode {
        const binary_node = createInit(alloc.*, BinaryNode, .{
            .op = op_token,
            .l = l,
            .r = r,
        }) catch
            return ParserError.MemoryError;
        return createInit(
            alloc.*,
            AstNode,
            .{ .AstBinaryNode = binary_node },
        ) catch
            return ParserError.MemoryError;
    }
    fn init_unary(
        alloc: *std.mem.Allocator,
        op_token: Token,
        r: *AstNode,
    ) ParserError!*AstNode {
        const unary_node = createInit(
            alloc.*,
            UnaryNode,
            .{ .op = op_token, .r = r },
        ) catch {
            return ParserError.MemoryError;
        };
        return createInit(
            alloc.*,
            AstNode,
            .{ .AstUnaryNode = unary_node },
        ) catch {
            return ParserError.MemoryError;
        };
    }
    fn init_literal(
        alloc: *std.mem.Allocator,
        literal: []const u8,
    ) ParserError!*AstNode {
        const literal_node = createInit(
            alloc.*,
            LiteralNode,
            .{ .literal = literal },
        ) catch {
            return ParserError.MemoryError;
        };
        return createInit(
            alloc.*,
            AstNode,
            .{ .AstLiteralNode = literal_node },
        ) catch {
            return ParserError.MemoryError;
        };
    }
};

pub const Parser = struct {
    allocator: *std.mem.Allocator,
    tokens: []Token,
    l: usize,
    r: usize,
    pub fn init(tokens: []Token, allocator: *std.mem.Allocator) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .l = 0,
            .r = 0,
        };
    }

    pub fn parse(self: *Parser, precedence: Precendence) ParserError!*AstNode {
        var node: ?*AstNode = null;
        while (self.r < self.tokens.len) {
            const token = self.tokens[self.r];
            if (token.type == TokenType.LPAREN) {
                self.r += 1;
                node = try self.parse(Precendence.LOWEST);
            } else if (token.type == TokenType.RPAREN) {
                if (node == null) {
                    node = try self.get_literal_node();
                }
                self.r += 1;
                return node.?;
            } else if (token.is_unary_operator()) {
                self.r += 1;
                node = try self.parse(token.get_precedence());
                return AstNode.init_unary(self.allocator, token, node.?);
            } else if (token.is_binary_operator()) {
                const new_precedence = token.get_precedence();
                if (@intFromEnum(new_precedence) < @intFromEnum(precedence)) {
                    if (node == null) {
                        node = try self.get_literal_node();
                    }
                    return node.?;
                } else {
                    if (node == null) {
                        node = try self.get_literal_node();
                    }
                    self.r += 1;
                    self.l = self.r;
                    const r = try self.parse(new_precedence);
                    const binary_node = try AstNode.init_binary(
                        self.allocator,
                        token,
                        node.?,
                        r,
                    );
                    node = binary_node;
                }
            } else {
                self.r += 1;
            }
        }

        if (node == null) {
            node = try self.get_literal_node();
        }
        return node.?;
    }

    fn get_literal_node(self: *Parser) ParserError!*AstNode {
        while (self.l < self.tokens.len and (self.tokens[self.l].type == TokenType.LPAREN or self.tokens[self.l].type == TokenType.BANG)) {
            self.l += 1;
        }
        return try AstNode.init_literal(
            self.allocator,
            self.tokens[self.l].literal,
        );
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var arena_allocator = arena.allocator();

    const src = "(10 + 10) * 100";
    var tokenizer = try Tokenizer.init(src, &arena_allocator);
    const tokens = try tokenizer.tokenize(&arena_allocator);
    var parser = Parser.init(tokens, &arena_allocator);
    const ast = try parser.parse(Precendence.LOWEST);
    const repr = ast.repr(&arena_allocator) catch {
        print("Error\n", .{});
        return;
    };
    print("Ast: {s}\n", .{repr});
}
