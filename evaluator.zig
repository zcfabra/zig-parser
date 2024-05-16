const std = @import("std");
const ast = @import("ast.zig");
const tokenizer = @import("tokenizer.zig");
const TokenType = tokenizer.TokenType;

const print = std.debug.print;

pub const EvalError = error{ OperandMismatch, InvalidOperator, InvalidData };
const LiteralType = union(enum) { BOOL, INT, STRING, FLOAT };
pub const EvaluatedLiteral = union(enum) {
    Bool: Bool,
    Int: Int,

    pub fn to_string(
        self: EvaluatedLiteral,
        allocator: *std.mem.Allocator,
    ) ![]const u8 {
        return switch (self) {
            .Bool => |b| b.to_string(),
            .Int => |i| i.to_string(allocator),
        } catch
            EvalError.InvalidData;
    }
};
pub const Bool = struct {
    value: bool,
    pub fn to_string(self: *const Bool) []const u8 {
        if (self.value) {
            return "True";
        }
        return "False";
    }
};
pub const Int = struct {
    value: i32,

    pub fn to_string(self: *const Int, allocator: *std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator.*, "{d}", .{self.value});
    }
};

pub const Evaluator = struct {
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) Evaluator {
        return Evaluator{
            .allocator = allocator,
        };
    }

    pub fn eval(self: *Evaluator, node: *ast.AstNode) EvalError!EvaluatedLiteral {
        return try switch (node.*) {
            ast.AstNode.AstLiteralNode => |literal| self.eval_literal(literal),
            ast.AstNode.AstUnaryNode => |unary| self.eval_unary(unary),
            ast.AstNode.AstBinaryNode => |binary| self.eval_binary(binary),
        };
    }

    fn eval_literal(_: *Evaluator, literal: *ast.LiteralNode) EvalError!EvaluatedLiteral {
        return switch (literal.token.type) {
            TokenType.NUM => {
                return EvaluatedLiteral{ .Int = Int{
                    .value = std.fmt.parseInt(
                        i32,
                        literal.token.literal,
                        10,
                    ) catch
                        return EvalError.InvalidData,
                } };
            },
            else => {
                return EvalError.InvalidData;
            },
        };
    }
    fn perform_binary_op(
        _: *Evaluator,
        _: TokenType,
        l: EvaluatedLiteral,
        r: EvaluatedLiteral,
    ) EvaluatedLiteral {
        return EvaluatedLiteral{
            .type = l.type,
            .value = l.value + r.value,
        };
    }
    fn eval_binary(
        self: *Evaluator,
        binary: *ast.BinaryNode,
    ) EvalError!EvaluatedLiteral {
        const l_expr = try self.eval(binary.l);
        const r_expr = try self.eval(binary.r);

        switch (binary.op.type) {
            TokenType.EQ => {
                switch (l_expr) {
                    EvaluatedLiteral.Int => |l| {
                        switch (r_expr) {
                            EvaluatedLiteral.Int => |r| {
                                return EvaluatedLiteral{
                                    .Bool = Bool{
                                        .value = l.value == r.value,
                                    },
                                };
                            },
                            else => return EvalError.InvalidData,
                        }
                    },
                    EvaluatedLiteral.Bool => |l| {
                        switch (r_expr) {
                            EvaluatedLiteral.Bool => |r| {
                                return EvaluatedLiteral{
                                    .Bool = Bool{
                                        .value = l.value == r.value,
                                    },
                                };
                            },
                            else => return EvalError.InvalidData,
                        }
                    },
                    // else => return EvalError.InvalidData,
                }
            },
            TokenType.ADD => {
                switch (l_expr) {
                    EvaluatedLiteral.Int => |l| {
                        switch (r_expr) {
                            EvaluatedLiteral.Int => |r| {
                                return EvaluatedLiteral{
                                    .Int = Int{ .value = l.value + r.value },
                                };
                            },
                            else => return EvalError.InvalidData,
                        }
                    },
                    else => return EvalError.OperandMismatch,
                }
            },

            TokenType.MUL => {
                switch (l_expr) {
                    EvaluatedLiteral.Int => |l| {
                        switch (r_expr) {
                            EvaluatedLiteral.Int => |r| {
                                return EvaluatedLiteral{ .Int = Int{
                                    .value = l.value * r.value,
                                } };
                            },
                            else => {
                                return EvalError.InvalidData;
                            },
                        }
                    },
                    else => {
                        return EvalError.InvalidData;
                    },
                }
            },
            else => {
                return EvalError.InvalidOperator;
            },
        }
    }

    fn eval_unary(
        _: *Evaluator,
        unary: *ast.UnaryNode,
    ) EvalError!EvaluatedLiteral {
        return switch (unary.op.type) {
            else => {
                return EvalError.InvalidData;
            },
        };
    }
};
