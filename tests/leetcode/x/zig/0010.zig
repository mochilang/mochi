const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn matchAt(s: []const u8, p: []const u8, i: usize, j: usize) bool {
    if (j >= p.len) return i >= s.len;
    const first = i < s.len and (p[j] == '.' or s[i] == p[j]);
    if (j + 1 < p.len and p[j + 1] == '*') {
        return matchAt(s, p, i, j + 2) or (first and matchAt(s, p, i + 1, j));
    }
    return first and matchAt(s, p, i + 1, j + 1);
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const s = std.mem.trim(u8, lines.next() orelse "", " \r\t");
        const p = std.mem.trim(u8, lines.next() orelse "", " \r\t");
        if (matchAt(s, p, 0, 0)) {
            _ = c.write(1, "true", 4);
        } else {
            _ = c.write(1, "false", 5);
        }
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
