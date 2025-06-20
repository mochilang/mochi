const std = @import("std");

fn letterCombinations(digits: []const u8) []const i32 {
	if (((digits).len == @as(i32,@intCast(0)))) {
		return [_]i32{};
	}
	const mapping = 0;
	var result = &[_]i32{""};
	for (digits) |d| {
		if (!((d in mapping))) {
			continue;
		}
		const letters = mapping[d];
		const next = 0;
		result = next;
	}
	return result;
}

pub fn main() void {
}
