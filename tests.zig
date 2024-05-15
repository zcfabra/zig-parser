const std = @import("std");
const main = @import("main.zig");

const print = std.debug.print;

test "test e2e" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var arena_allocator = arena.allocator();

    const src = "(10 + 10) * 100";
    var tokenizer = try main.Tokenizer.init(src, &arena_allocator);
    const tokens = try tokenizer.tokenize(&arena_allocator);
    var parser = main.Parser.init(tokens, &arena_allocator);
    const ast = try parser.parse(main.Precendence.LOWEST);
    _ = ast.repr(&arena_allocator) catch {
        print("Error\n", .{});
        return;
    };
}

test "test nested brackets" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var arena_allocator = arena.allocator();

    const src = "((10 + 10) * 100) * ((10 + 10) * 900)";
    var tokenizer = try main.Tokenizer.init(src, &arena_allocator);
    const tokens = try tokenizer.tokenize(&arena_allocator);
    var parser = main.Parser.init(tokens, &arena_allocator);
    const ast = try parser.parse(main.Precendence.LOWEST);
    _ = ast.repr(&arena_allocator) catch {
        print("Error\n", .{});
        return;
    };
}
