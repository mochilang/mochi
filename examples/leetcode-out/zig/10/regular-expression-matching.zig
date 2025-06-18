const std = @import("std");

fn isMatch(s: []const u8, p: []const u8) [2]i32 {
	const m = s.len;
	const n = p.len;
	var dp = std.ArrayList(i32).init(std.heap.page_allocator);
	var i = 0;
	while ((i <= m)) {
		var row = std.ArrayList(i32).init(std.heap.page_allocator);
		var j = 0;
		while ((j <= n)) {
			try row.append(@as(i32,@intCast(false)));
			j = (j + 1);
		}
		try dp.append(@as(i32,@intCast(row)));
		i = (i + 1);
	}
	dp[m][n] = true;
	var i2 = m;
	while ((i2 >= 0)) {
		var j2 = (n - 1);
		while ((j2 >= 0)) {
			var first = false;
			if ((i2 < m)) {
				if ((((p[j2] == s[i2])) or ((p[j2] == ".")))) {
					first = true;
				}
			}
			if (((((j2 + 1) < n) and p[(j2 + 1)]) == "*")) {
				if ((dp[i2][(j2 + 2)] or ((first and dp[(i2 + 1)][j2])))) {
					dp[i2][j2] = true;
				} else {
					dp[i2][j2] = false;
				}
			} else {
				if ((first and dp[(i2 + 1)][(j2 + 1)])) {
					dp[i2][j2] = true;
				} else {
					dp[i2][j2] = false;
				}
			}
			j2 = (j2 - 1);
		}
		i2 = (i2 - 1);
	}
	return dp[0][0];
}

pub fn main() void {
}
