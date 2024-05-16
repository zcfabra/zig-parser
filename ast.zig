const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const utils = @import("utils.zig");

pub const LiteralNode = struct {
    token: tokenizer.Token,

    fn repr(self: *LiteralNode) []const u8 {
        return self.token.literal;
    }
};
pub const BinaryNode = struct {
    l: *AstNode,
    r: *AstNode,
    op: tokenizer.Token,

    fn repr(
        self: *BinaryNode,
        allocator: *std.mem.Allocator,
    ) utils.MemoryError![]const u8 {
        const l = try self.l.repr(allocator);
        const r = try self.r.repr(allocator);
        // defer allocator.free(l);
        // defer allocator.free(r);
        return std.fmt.allocPrint(
            allocator.*,
            "( {s} {s} {s} )",
            .{ l, self.op.literal, r },
        ) catch
            utils.MemoryError.AllocationError;
    }
};

pub const UnaryNode = struct {
    r: *AstNode,
    op: tokenizer.Token,

    fn repr(self: *UnaryNode, allocator: *std.mem.Allocator) utils.MemoryError![]const u8 {
        const r = try self.r.repr(allocator);
        // defer allocator.free(r);
        const str = std.fmt.allocPrint(
            allocator.*,
            "( {s} {s} )",
            .{ self.op.literal, r },
        ) catch
            return utils.MemoryError.AllocationError;
        return str;
    }
};

pub const AstNode = union(enum) {
    AstLiteralNode: *LiteralNode,
    AstBinaryNode: *BinaryNode,
    AstUnaryNode: *UnaryNode,

    pub fn repr(self: AstNode, allocator: *std.mem.Allocator) utils.MemoryError![]const u8 {
        return switch (self) {
            AstNode.AstLiteralNode => |literal| literal.repr(),
            AstNode.AstUnaryNode => |unary| try unary.repr(allocator),
            AstNode.AstBinaryNode => |binary| try binary.repr(allocator),
        };
    }
    pub fn init_binary(
        alloc: *std.mem.Allocator,
        op_token: tokenizer.Token,
        l: *AstNode,
        r: *AstNode,
    ) utils.MemoryError!*AstNode {
        const binary_node = utils.createInit(alloc.*, BinaryNode, .{
            .op = op_token,
            .l = l,
            .r = r,
        }) catch
            return utils.MemoryError.AllocationError;
        return utils.createInit(
            alloc.*,
            AstNode,
            .{ .AstBinaryNode = binary_node },
        ) catch
            return utils.MemoryError.AllocationError;
    }
    pub fn init_unary(
        alloc: *std.mem.Allocator,
        op_token: tokenizer.Token,
        r: *AstNode,
    ) utils.MemoryError!*AstNode {
        const unary_node = utils.createInit(
            alloc.*,
            UnaryNode,
            .{ .op = op_token, .r = r },
        ) catch {
            return utils.MemoryError.AllocationError;
        };
        return utils.createInit(
            alloc.*,
            AstNode,
            .{ .AstUnaryNode = unary_node },
        ) catch {
            return utils.MemoryError.AllocationError;
        };
    }
    pub fn init_literal(
        alloc: *std.mem.Allocator,
        token: tokenizer.Token,
    ) utils.MemoryError!*AstNode {
        const literal_node = utils.createInit(
            alloc.*,
            LiteralNode,
            .{ .token = token },
        ) catch {
            return utils.MemoryError.AllocationError;
        };
        return utils.createInit(
            alloc.*,
            AstNode,
            .{ .AstLiteralNode = literal_node },
        ) catch {
            return utils.MemoryError.AllocationError;
        };
    }
};
