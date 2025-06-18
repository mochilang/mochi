const std = @import("std");

fn expand(s: []const u8, left: i32, right: i32) [2]i32 {
	var l = left;
	var r = right;
	const n = s.len;
	while ((((l >= 0) and r) < n)) {
		if ((s[l] != s[r])) {
			break;
		}
		l = (l - 1);
		r = (r + 1);
	}
	return ((r - l) - 1);
}

fn longestPalindrome(s: []const u8) [2]i32 {
	if ((s.len <= 1)) {
		return s;
	}
	var start = 0;
	var end = 0;
	const n = s.len;
	for (0 .. n) |i| {
		const len1 = expand(s, i, i);
		const len2 = expand(s, i, (i + 1));
		var l = len1;
		if ((len2 > len1)) {
			l = len2;
		}
		if (((l > end) - start)) {
			start = ((i - ((l - 1))) / 2);
			end = ((i + l) / 2);
		}
	}
	return s[start];
}

pub fn main() void {
}
