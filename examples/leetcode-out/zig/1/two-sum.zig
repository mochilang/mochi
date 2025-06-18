const std = @import("std");

fn twoSum(nums: []const i32, target: i32) [2]i32 {
	const n = nums.len;
	for (0 .. n) |i| {
		for ((i + 1) .. n) |j| {
			if (((nums[i] + nums[j]) == target)) {
				return [_]i32{@as(i32,@intCast(i)), @as(i32,@intCast(j))};
			}
		}
	}
	return [_]i32{@as(i32,@intCast(-1)), @as(i32,@intCast(-1))};
}

pub fn main() void {
	const result = twoSum(&[_]i32{@as(i32,@intCast(2)), @as(i32,@intCast(7)), @as(i32,@intCast(11)), @as(i32,@intCast(15))}, 9);
	std.debug.print("{}\n", .{result[0]});
	std.debug.print("{}\n", .{result[1]});
}
