const std = @import("std");

fn digit(ch: []const u8) [2]i32 {
	if (std.mem.eql(u8, ch, "0")) {
		return 0;
	}
	if (std.mem.eql(u8, ch, "1")) {
		return 1;
	}
	if (std.mem.eql(u8, ch, "2")) {
		return 2;
	}
	if (std.mem.eql(u8, ch, "3")) {
		return 3;
	}
	if (std.mem.eql(u8, ch, "4")) {
		return 4;
	}
	if (std.mem.eql(u8, ch, "5")) {
		return 5;
	}
	if (std.mem.eql(u8, ch, "6")) {
		return 6;
	}
	if (std.mem.eql(u8, ch, "7")) {
		return 7;
	}
	if (std.mem.eql(u8, ch, "8")) {
		return 8;
	}
	if (std.mem.eql(u8, ch, "9")) {
		return 9;
	}
	return -1;
}

fn myAtoi(s: []const u8) [2]i32 {
	var i = 0;
	const n = (s).len;
	while ((((i < n) and s[i]) == " "[0])) {
		i = (i + 1);
	}
	var sign = 1;
	if (((i < n) and ((((s[i] == "+"[0]) or s[i]) == "-"[0])))) {
		if ((s[i] == "-"[0])) {
			sign = -1;
		}
		i = (i + 1);
	}
	var result = 0;
	while ((i < n)) {
		const ch = s[i..(i + 1)];
		const d = digit(ch);
		if ((d < 0)) {
			break;
		}
		result = ((result * 10) + d);
		i = (i + 1);
	}
	result = (result * sign);
	if ((result > 2147483647)) {
		return 2147483647;
	}
	if ((result < (-2147483648))) {
		return -2147483648;
	}
	return result;
}

pub fn main() void {
}
