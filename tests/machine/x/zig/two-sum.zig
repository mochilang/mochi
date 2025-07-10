const std = @import("std");

const result = twoSum(&[_]i32{
    2,
    7,
    11,
    15,
}, 9);

fn twoSum(nums: []const i32, target: i32) []const i32 {
    const n = (nums).len;
    for (0 .. n) |i| {
        for ((i + 1) .. n) |j| {
            if (((nums[i] + nums[j]) == target)) {
                return [_]i32{
    i,
    j,
};
            }
        }
    }
    return [_]i32{
    -1,
    -1,
};
}

pub fn main() void {
    std.debug.print("{d}\n", .{result[0]});
    std.debug.print("{d}\n", .{result[1]});
}
