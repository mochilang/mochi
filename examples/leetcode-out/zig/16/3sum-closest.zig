const std = @import("std");

fn threeSumClosest(nums: []const i32, target: i32) i32 {
	const sorted = 0;
	const n = (sorted).len;
	var best = ((sorted[@as(i32,@intCast(0))] + sorted[@as(i32,@intCast(1))]) + sorted[@as(i32,@intCast(2))]);
	for (@as(i32,@intCast(0)) .. n) |i| {
		var left = (i + @as(i32,@intCast(1)));
		var right = (n - @as(i32,@intCast(1)));
		while ((left < right)) {
			const sum = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == target)) {
				return target;
			}
			var diff = @as(i32,@intCast(0));
			if ((sum > target)) {
				diff = (sum - target);
			} else {
				diff = (target - sum);
			}
			var bestDiff = @as(i32,@intCast(0));
			if ((best > target)) {
				bestDiff = (best - target);
			} else {
				bestDiff = (target - best);
			}
			if ((diff < bestDiff)) {
				best = sum;
			}
			if ((sum < target)) {
				left = (left + @as(i32,@intCast(1)));
			} else {
				right = (right - @as(i32,@intCast(1)));
			}
		}
	}
	return best;
}

pub fn main() void {
}
