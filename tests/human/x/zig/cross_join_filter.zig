const std = @import("std");

const Pair = struct {
    n: i32,
    l: []const u8,
};

pub fn main() !void {
    const nums = [_]i32{1, 2, 3};
    const letters = [_][]const u8{"A", "B"};

    var pairs = std.ArrayList(Pair).init(std.heap.page_allocator);
    defer pairs.deinit();

    for (nums) |n| {
        if (n % 2 == 0) {
            for (letters) |l| {
                try pairs.append(.{ .n = n, .l = l });
            }
        }
    }

    std.debug.print("--- Even pairs ---\n", .{});
    for (pairs.items) |p| {
        std.debug.print("{d} {s}\n", .{ p.n, p.l });
    }
}

