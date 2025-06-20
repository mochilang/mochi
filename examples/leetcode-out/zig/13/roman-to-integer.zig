const std = @import("std");

fn romanToInt(s: []const u8) i32 {
	const values = 0;
	var total = @as(i32,@intCast(0));
	var i = @as(i32,@intCast(0));
	const n = (s).len;
	while ((i < n)) {
		const curr = values[s[i]];
		if (((i + @as(i32,@intCast(1))) < n)) {
			const next = values[s[(i + @as(i32,@intCast(1)))]];
			if ((curr < next)) {
				total = ((total + next) - curr);
				i = (i + @as(i32,@intCast(2)));
				continue;
			}
		}
		total = (total + curr);
		i = (i + @as(i32,@intCast(1)));
	}
	return total;
}

pub fn main() void {
}
