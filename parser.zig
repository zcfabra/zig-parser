const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const utils = @import("utils.zig");
const ast = @import("ast.zig");

const ParserError = error{
    MemoryError,
};

pub const Precendence = enum(i32) {
    LOWEST,
    ADDSUB,
    DIVMUL,
    BANG,
};

pub const Parser = struct {
    allocator: *std.mem.Allocator,
    tokens: []tokenizer.Token,
    l: usize,
    r: usize,
    pub fn init(tokens: []tokenizer.Token, allocator: *std.mem.Allocator) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .l = 0,
            .r = 0,
        };
    }

    pub fn parse(self: *Parser, precedence: Precendence) ParserError!*ast.AstNode {
        var node: ?*ast.AstNode = null;
        while (self.r < self.tokens.len) {
            const token = self.tokens[self.r];
            if (token.type == tokenizer.TokenType.LPAREN) {
                self.r += 1;
                node = try self.parse(Precendence.LOWEST);
            } else if (token.type == tokenizer.TokenType.RPAREN) {
                if (node == null) {
                    node = try self.get_literal_node();
                }
                self.r += 1;
                return node.?;
            } else if (token.is_unary_operator()) {
                self.r += 1;
                node = try self.parse(token.get_precedence());
                return ast.AstNode.init_unary(
                    self.allocator,
                    token,
                    node.?,
                ) catch
                    ParserError.MemoryError;
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
                    const binary_node = ast.AstNode.init_binary(
                        self.allocator,
                        token,
                        node.?,
                        r,
                    ) catch
                        return ParserError.MemoryError;
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

    fn get_literal_node(self: *Parser) ParserError!*ast.AstNode {
        while (self.l < self.tokens.len and (self.tokens[self.l].type == tokenizer.TokenType.LPAREN or self.tokens[self.l].type == tokenizer.TokenType.BANG)) {
            self.l += 1;
        }
        return ast.AstNode.init_literal(
            self.allocator,
            self.tokens[self.l],
        ) catch ParserError.MemoryError;
    }
};
