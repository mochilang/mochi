const std = @import("std");

fn threeSum(nums: []const i32) []const i32 {
	const sorted = 0;
	const n = (sorted).len;
	var res = std.ArrayList(i32).init(std.heap.page_allocator);
	var i = @as(i32,@intCast(0));
	while ((i < n)) {
		if ((((i > @as(i32,@intCast(0))) and sorted[i]) == sorted[(i - @as(i32,@intCast(1)))])) {
			i = (i + @as(i32,@intCast(1)));
			continue;
		}
		var left = (i + @as(i32,@intCast(1)));
		var right = (n - @as(i32,@intCast(1)));
		while ((left < right)) {
			const sum = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == @as(i32,@intCast(0)))) {
				try res.append(@as(i32,@intCast(&[_]i32{sorted[i], sorted[left], sorted[right]})));
				left = (left + @as(i32,@intCast(1)));
				while ((((left < right) and sorted[left]) == sorted[(left - @as(i32,@intCast(1)))])) {
					left = (left + @as(i32,@intCast(1)));
				}
				right = (right - @as(i32,@intCast(1)));
				while ((((left < right) and sorted[right]) == sorted[(right + @as(i32,@intCast(1)))])) {
					right = (right - @as(i32,@intCast(1)));
				}
			} else 			if ((sum < @as(i32,@intCast(0)))) {
				left = (left + @as(i32,@intCast(1)));
			} else {
				right = (right - @as(i32,@intCast(1)));
			}
		}
		i = (i + @as(i32,@intCast(1)));
	}
	return res;
}

pub fn main() void {
}
