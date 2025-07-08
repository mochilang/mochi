const std = @import("std");

var nums: []const i32 = undefined;

pub fn main() void {
    nums = &[_]i32{1, 2};
    nums.items[1] = 3;
    std.debug.print("{any}\n", .{nums[1]});
}
