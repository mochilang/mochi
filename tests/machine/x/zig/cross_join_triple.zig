const std = @import("std");

var nums: []const i32 = undefined;
var letters: []const []const u8 = undefined;
var bools: []const bool = undefined;
var combos: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    nums = &[_]i32{1, 2};
    letters = &[_][]const u8{"A", "B"};
    bools = &[_]bool{true, false};
    combos = blk1: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (nums) |n| { for (letters) |l| { for (bools) |b| { _tmp0.append(blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(n, n) catch unreachable; m.put(l, l) catch unreachable; m.put(b, b) catch unreachable; break :blk0 m; }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk1 _tmp1; };
    std.debug.print("{s}\n", .{"--- Cross Join of three lists ---"});
    for (combos) |c| {
        std.debug.print("{any} {any} {any}\n", .{c.n, c.l, c.b});
    }
}
