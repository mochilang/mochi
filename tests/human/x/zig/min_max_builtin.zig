const std = @import("std");

pub fn main() void {
    const nums = [_]i32{3,1,4};
    var minv = nums[0];
    var maxv = nums[0];
    for (nums[1..]) |n| {
        if (n < minv) minv = n;
        if (n > maxv) maxv = n;
    }
    std.debug.print("{d}\n", .{minv});
    std.debug.print("{d}\n", .{maxv});
}
