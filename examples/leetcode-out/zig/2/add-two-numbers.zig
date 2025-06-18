const std = @import("std");

fn addTwoNumbers(l1: []const i32, l2: []const i32) [2]i32 {
	var i = 0;
	var j = 0;
	var carry = 0;
	var result = std.ArrayList(i32).init(std.heap.page_allocator);
	while ((((((i < l1.len) or j) < l2.len) or carry) > 0)) {
		var x = 0;
		if ((i < l1.len)) {
			x = l1[i];
			i = (i + 1);
		}
		var y = 0;
		if ((j < l2.len)) {
			y = l2[j];
			j = (j + 1);
		}
		const sum = ((x + y) + carry);
		const digit = (sum % 10);
		carry = (sum / 10);
		try result.append(@as(i32,@intCast(digit)));
	}
	return result;
}

pub fn main() void {
}
