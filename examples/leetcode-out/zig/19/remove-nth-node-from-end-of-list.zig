const std = @import("std");

fn removeNthFromEnd(nums: []const i32, n: i32) []const i32 {
	const idx = ((nums).len - n);
	var result = std.ArrayList(i32).init(std.heap.page_allocator);
	var i = @as(i32,@intCast(0));
	while ((i < (nums).len)) {
		if ((i != idx)) {
			try result.append(@as(i32,@intCast(nums[i])));
		}
		i = (i + @as(i32,@intCast(1)));
	}
	return result;
}

pub fn main() void {
}
