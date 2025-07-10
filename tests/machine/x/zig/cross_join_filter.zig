const std = @import("std");

const nums = &[_]i32{1, 2, 3};
const letters = &[_][]const u8{"A", "B"};
const pairs = blk0: { var _tmp0 = std.ArrayList(struct { n: i32, l: []const u8, }).init(std.heap.page_allocator); for (nums) |n| { for (letters) |l| { if (!((@mod(n, 2) == 0))) continue; _tmp0.append(struct { n: i32, l: []const u8, }{ .n = n, .l = l }) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; };

pub fn main() void {
    std.debug.print("{s}\n", .{"--- Even pairs ---"});
    for (pairs) |p| {
        std.debug.print("{any} {any}\n", .{p.n, p.l});
    }
}
