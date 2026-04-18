const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }

fn solve(s: []const u8, t: []const u8) i32 {
    var dp: [1005]i32 = undefined;
    for (0..1005) |i| dp[i] = 0;
    dp[0] = 1;
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        var j: usize = t.len;
        while (j >= 1) : (j -= 1) {
            if (s[i] == t[j - 1]) dp[j] += dp[j - 1];
            if (j == 1) break;
        }
    }
    return dp[t.len];
}

pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const tc = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var i: usize = 0;
    while (i < tc) : (i += 1) {
        const s = std.mem.trim(u8, lines.next() orelse "", "\r");
        const t = std.mem.trim(u8, lines.next() orelse "", "\r");
        var outbuf: [32]u8 = undefined;
        const out = try std.fmt.bufPrint(&outbuf, "{}", .{solve(s, t)});
        writeAll(out);
        if (i + 1 < tc) writeAll("\n");
    }
}
