const std = @import("std");

fn convert(s: []const u8, numRows: i32) [2]i32 {
	if ((((numRows <= 1) or numRows) >= s.len)) {
		return s;
	}
	var rows = std.ArrayList(i32).init(std.heap.page_allocator);
	var i = 0;
	while ((i < numRows)) {
		try rows.append(@as(i32,@intCast("")));
		i = (i + 1);
	}
	var curr = 0;
	var step = 1;
	for (s) |ch| {
		rows[curr] = (rows[curr] + ch);
		if ((curr == 0)) {
			step = 1;
		} else 		if (((curr == numRows) - 1)) {
			step = -1;
		}
		curr = (curr + step);
	}
	var result = "";
	for (rows) |row| {
		result = (result + row);
	}
	return result;
}

pub fn main() void {
}
