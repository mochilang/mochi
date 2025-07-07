const std = @import("std");

var result: []const i32 = undefined;

fn twoSum(nums: []const i32, target: i32) []const i32 {
    const n = (nums).len;
    for (@as(i32, @intCast(0))..n) |i| {
        for ((i + @as(i32, @intCast(1)))..n) |j| {
            if (((nums[i] + nums[j]) == target)) {
                return [_]i32{ i, j };
            }
        }
    }
    return [_]i32{ -@as(i32, @intCast(1)), -@as(i32, @intCast(1)) };
}

pub fn main() void {
    result = twoSum(&[_]i32{ @as(i32, @intCast(2)), @as(i32, @intCast(7)), @as(i32, @intCast(11)), @as(i32, @intCast(15)) }, @as(i32, @intCast(9)));
    std.debug.print("{any}\n", .{result[@as(i32, @intCast(0))]});
    std.debug.print("{any}\n", .{result[@as(i32, @intCast(1))]});
}
