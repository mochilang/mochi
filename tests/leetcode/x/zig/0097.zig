const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }

fn solve(s1: []const u8, s2: []const u8, s3: []const u8) bool {
    const m = s1.len;
    const n = s2.len;
    if (m + n != s3.len) return false;
    var dp: [128][128]bool = undefined;
    for (0..128) |i| {
        for (0..128) |j| {
            dp[i][j] = false;
        }
    }
    dp[0][0] = true;
    var i: usize = 0;
    while (i <= m) : (i += 1) {
        var j: usize = 0;
        while (j <= n) : (j += 1) {
            if (i > 0 and dp[i - 1][j] and s1[i - 1] == s3[i + j - 1]) dp[i][j] = true;
            if (j > 0 and dp[i][j - 1] and s2[j - 1] == s3[i + j - 1]) dp[i][j] = true;
        }
    }
    return dp[m][n];
}

pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const s1 = std.mem.trim(u8, lines.next() orelse "", "\r");
        const s2 = std.mem.trim(u8, lines.next() orelse "", "\r");
        const s3 = std.mem.trim(u8, lines.next() orelse "", "\r");
        writeAll(if (solve(s1, s2, s3)) "true" else "false");
        if (tc + 1 < t) writeAll("\n");
    }
}
