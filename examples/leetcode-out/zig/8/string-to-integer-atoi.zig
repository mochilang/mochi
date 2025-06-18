const std = @import("std");

fn myAtoi(s: []const u8) [2]i32 {
	var i = 0;
	const n = s.len;
	while ((((i < n) and s[i]) == " ")) {
		i = (i + 1);
	}
	var sign = 1;
	if (((i < n) and ((((s[i] == "+") or s[i]) == "-")))) {
		if ((s[i] == "-")) {
			sign = -1;
		}
		i = (i + 1);
	}
	const digits = 0;
	var result = 0;
	while ((i < n)) {
		const ch = s[i];
		if (!((ch in digits))) {
			break;
		}
		const d = digits[ch];
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
