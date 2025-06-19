const std = @import("std");

fn lengthOfLongestSubstring(s: []const u8) [2]i32 {
	const n = (s).len;
	var start = 0;
	var best = 0;
	var i = 0;
	while ((i < n)) {
		var j = start;
		while ((j < i)) {
			if ((s[j] == s[i])) {
				start = (j + 1);
				break;
			}
			j = (j + 1);
		}
		const length = ((i - start) + 1);
		if ((length > best)) {
			best = length;
		}
		i = (i + 1);
	}
	return best;
}

pub fn main() void {
}
