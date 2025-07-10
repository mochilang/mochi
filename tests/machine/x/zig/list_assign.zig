const std = @import("std");

var nums = &[_]i32{
    1,
    2,
};

pub fn main() void {
    nums.items[1] = 3;
    std.debug.print("{d}\n", .{nums[1]});
}
