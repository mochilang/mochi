const std = @import("std");

pub fn main() void {
    const nums = [_]i32{1,2,3};
    var sum: i32 = 0;
    for (nums) |v| sum += v;
    const avg = sum / nums.len;
    std.debug.print("{d}\n", .{avg});
}
