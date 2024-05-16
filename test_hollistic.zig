const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");

const print = std.debug.print;

test "test e2e" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var arena_allocator = arena.allocator();

    const src = "(10 + 10) * 100";
    var tk = try tokenizer.Tokenizer.init(src, &arena_allocator);
    const tokens = try tk.tokenize(&arena_allocator);
    var prs = parser.Parser.init(tokens, &arena_allocator);
    const ast = try prs.parse(parser.Precendence.LOWEST);
    _ = ast.repr(&arena_allocator) catch {
        print("Error\n", .{});
        return;
    };
}

test "test nested brackets" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    const src = "((10 + 10) * 100) * ((10 + 10) * 900)";
    var tk = try tokenizer.Tokenizer.init(src, &allocator);
    const tokens = try tk.tokenize(&allocator);
    var prs = parser.Parser.init(tokens, &allocator);
    const ast = try prs.parse(parser.Precendence.LOWEST);
    print("\nAST: {s}\n", .{try ast.repr(&allocator)});
    var evl = evaluator.Evaluator.init(&allocator);
    const res = try evl.eval(ast);
    print("Eval: {s}\n", .{try res.to_string(&allocator)});
}
