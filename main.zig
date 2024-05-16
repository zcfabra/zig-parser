const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");

const ArrayList = std.ArrayList;
const print = std.debug.print;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var arena_allocator = arena.allocator();

    const src = "10 + 10";
    var tk = try tokenizer.Tokenizer.init(src, &arena_allocator);
    const tokens = try tk.tokenize(&arena_allocator);
    var prs = parser.Parser.init(tokens, &arena_allocator);
    const ast = try prs.parse(parser.Precendence.LOWEST);
    const repr = ast.repr(&arena_allocator) catch {
        print("Error\n", .{});
        return;
    };
    print("Ast: {s}\n", .{repr});

    var evl = evaluator.Evaluator.init(&arena_allocator);
    const result = try evl.eval(ast);
    print("{s}\n", .{try result.to_string(&arena_allocator)});
}
