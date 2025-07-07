const std = @import("std");

pub fn main() void {
    const nums = [_]i32{1,2,3};
    var total: i32 = 0;
    for (nums) |n| {
        if (n > 1) total += n;
    }
    std.debug.print("{d}\n", .{total});
}
