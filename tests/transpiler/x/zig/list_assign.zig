// Generated by Mochi Zig transpiler on 2025-07-22 09:03 +0700
const std = @import("std");

pub fn main() void {
    var nums = [2]i64{1, 2};
    nums[1] = 3;
    std.io.getStdOut().writer().print("{d}\n", .{nums[1]}) catch unreachable;
}
