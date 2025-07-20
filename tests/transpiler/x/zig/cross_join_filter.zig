const std = @import("std");

const Pair = struct {
    n: i32,
    l: []const u8,
};

pub fn main() !void {
    const nums = [_]i32{1, 2, 3};
    const letters = [_][]const u8{"A", "B"};

    const pairs = blk: {
        var arr = std.ArrayList(Pair).init(std.heap.page_allocator);
        for (nums) |n| {
            for (letters) |l| {
                if ((n % 2) == 0) {
                    arr.append(.{ .n = n, .l = l }) catch unreachable;
                }
            }
        }
        const tmp = arr.toOwnedSlice() catch unreachable;
        break :blk tmp;
    };

    try std.io.getStdOut().writer().print("--- Even pairs ---\n", .{});
    for (pairs) |p| {
        try std.io.getStdOut().writer().print("{d} {s}\n", .{ p.n, p.l });
    }
}
