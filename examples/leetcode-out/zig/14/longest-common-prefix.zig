const std = @import("std");

fn longestCommonPrefix(strs: []const i32) []const u8 {
	if (((strs).len == @as(i32,@intCast(0)))) {
		return "";
	}
	var prefix = strs[@as(i32,@intCast(0))];
	for (@as(i32,@intCast(1)) .. (strs).len) |i| {
		var j = @as(i32,@intCast(0));
		const current = strs[i];
		while ((((j < (prefix).len) and j) < (current).len)) {
			if ((prefix[j] != current[j])) {
				break;
			}
			j = (j + @as(i32,@intCast(1)));
		}
		prefix = prefix[@as(i32,@intCast(0))..j];
		if (std.mem.eql(u8, prefix, "")) {
			break;
		}
	}
	return prefix;
}

pub fn main() void {
}
