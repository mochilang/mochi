const std = @import("std");

const Combo = struct {
    n: i32,
    l: []const u8,
    b: bool,
};

pub fn main() !void {
    const nums = [_]i32{1, 2};
    const letters = [_][]const u8{"A", "B"};
    const bools = [_]bool{ true, false };

    var combos = std.ArrayList(Combo).init(std.heap.page_allocator);
    defer combos.deinit();

    for (nums) |n| {
        for (letters) |l| {
            for (bools) |b| {
                try combos.append(.{ .n = n, .l = l, .b = b });
            }
        }
    }

    std.debug.print("--- Cross Join of three lists ---\n", .{});
    for (combos.items) |c| {
        std.debug.print("{d} {s} {}\n", .{ c.n, c.l, c.b });
    }
}

