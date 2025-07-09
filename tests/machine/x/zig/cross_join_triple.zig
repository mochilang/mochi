const std = @import("std");

const nums = &[_]i32{1, 2};
const letters = &[_][]const u8{"A", "B"};
const bools = &[_]bool{true, false};
const combos = blk0: { var _tmp0 = std.ArrayList(struct { n: i32 l: []const u8 b: bool }).init(std.heap.page_allocator); for (nums) |n| { for (letters) |l| { for (bools) |b| { _tmp0.append(struct { n: i32, l: []const u8, b: bool, }{ .n = n, .l = l, .b = b }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; };

pub fn main() void {
    std.debug.print("{s}\n", .{"--- Cross Join of three lists ---"});
    for (combos) |c| {
        std.debug.print("{any} {any} {any}\n", .{c.n, c.l, c.b});
    }
}
