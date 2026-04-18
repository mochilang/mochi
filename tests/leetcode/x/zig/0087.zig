const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }

var s1g: []const u8 = undefined;
var s2g: []const u8 = undefined;
var memo: [31][31][31]i8 = undefined;

fn dfs(a1: usize, a2: usize, len: usize) bool {
    if (memo[a1][a2][len] != -1) return memo[a1][a2][len] == 1;
    if (std.mem.eql(u8, s1g[a1 .. a1 + len], s2g[a2 .. a2 + len])) { memo[a1][a2][len] = 1; return true; }
    var cnt: [26]i32 = [_]i32{0} ** 26;
    for (0..len) |i| {
        cnt[s1g[a1 + i] - 'a'] += 1;
        cnt[s2g[a2 + i] - 'a'] -= 1;
    }
    for (cnt) |v| if (v != 0) { memo[a1][a2][len] = 0; return false; };
    var k: usize = 1;
    while (k < len) : (k += 1) {
        if ((dfs(a1, a2, k) and dfs(a1 + k, a2 + k, len - k)) or (dfs(a1, a2 + len - k, k) and dfs(a1 + k, a2, len - k))) {
            memo[a1][a2][len] = 1;
            return true;
        }
    }
    memo[a1][a2][len] = 0;
    return false;
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
        s1g = std.mem.trim(u8, lines.next() orelse "", "\r");
        s2g = std.mem.trim(u8, lines.next() orelse "", "\r");
        for (0..31) |i| {
            for (0..31) |j| {
                for (0..31) |k| {
                    memo[i][j][k] = -1;
                }
            }
        }
        if (dfs(0, 0, s1g.len)) writeAll("true") else writeAll("false");
        if (tc + 1 < t) writeAll("\n");
    }
}
