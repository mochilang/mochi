const std = @import("std");

fn isValid(s: []const u8) bool {
	var stack = std.ArrayList(i32).init(std.heap.page_allocator);
	const n = (s).len;
	for (@as(i32,@intCast(0)) .. n) |i| {
		const c = s[i];
		if (std.mem.eql(u8, c, "(")) {
			try stack.append(@as(i32,@intCast(")")));
		} else 		if (std.mem.eql(u8, c, "[")) {
			try stack.append(@as(i32,@intCast("]")));
		} else 		if (std.mem.eql(u8, c, "{")) {
			try stack.append(@as(i32,@intCast("}")));
		} else {
			if (((stack).len == @as(i32,@intCast(0)))) {
				return false;
			}
			const top = stack[((stack).len - @as(i32,@intCast(1)))];
			if ((top != c)) {
				return false;
			}
			stack = stack[@as(i32,@intCast(0))..((stack).len - @as(i32,@intCast(1)))];
		}
	}
	return ((stack).len == @as(i32,@intCast(0)));
}

pub fn main() void {
}
