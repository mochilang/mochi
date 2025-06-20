const std = @import("std");

fn fourSum(nums: []const i32, target: i32) []const i32 {
	const sorted = 0;
	const n = (sorted).len;
	var result = std.ArrayList(i32).init(std.heap.page_allocator);
	for (@as(i32,@intCast(0)) .. n) |i| {
		if ((((i > @as(i32,@intCast(0))) and sorted[i]) == sorted[(i - @as(i32,@intCast(1)))])) {
			continue;
		}
		for ((i + @as(i32,@intCast(1))) .. n) |j| {
			if (((((j > i) + @as(i32,@intCast(1))) and sorted[j]) == sorted[(j - @as(i32,@intCast(1)))])) {
				continue;
			}
			var left = (j + @as(i32,@intCast(1)));
			var right = (n - @as(i32,@intCast(1)));
			while ((left < right)) {
				const sum = (((sorted[i] + sorted[j]) + sorted[left]) + sorted[right]);
				if ((sum == target)) {
					try result.append(@as(i32,@intCast(&[_]i32{sorted[i], sorted[j], sorted[left], sorted[right]})));
					left = (left + @as(i32,@intCast(1)));
					right = (right - @as(i32,@intCast(1)));
					while ((((left < right) and sorted[left]) == sorted[(left - @as(i32,@intCast(1)))])) {
						left = (left + @as(i32,@intCast(1)));
					}
					while ((((left < right) and sorted[right]) == sorted[(right + @as(i32,@intCast(1)))])) {
						right = (right - @as(i32,@intCast(1)));
					}
				} else 				if ((sum < target)) {
					left = (left + @as(i32,@intCast(1)));
				} else {
					right = (right - @as(i32,@intCast(1)));
				}
			}
		}
	}
	return result;
}

pub fn main() void {
}
