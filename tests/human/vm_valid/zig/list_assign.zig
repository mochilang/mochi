const std = @import("std");

pub fn main() void {
    var nums = [_]i32{1, 2};
    nums[1] = 3;
    std.debug.print("{d}\n", .{nums[1]});
}
