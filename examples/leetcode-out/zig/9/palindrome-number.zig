const std = @import("std");

fn isPalindrome(x: i32) [2]i32 {
	if ((x < 0)) {
		return false;
	}
	const s = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{x}) catch unreachable;
	const n = (s).len;
	for (0 .. (n / 2)) |i| {
		if ((s[i] != s[((n - 1) - i)])) {
			return false;
		}
	}
	return true;
}

pub fn main() void {
}
