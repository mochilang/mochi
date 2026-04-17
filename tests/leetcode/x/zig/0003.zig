const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn longest(s: []const u8) i64 {
    var last: [256]i64 = undefined;
    for (0..256) |i| last[i] = -1;
    var left: i64 = 0;
    var best: i64 = 0;
    for (s, 0..) |ch, right_usize| {
        const right: i64 = @intCast(right_usize);
        if (last[ch] >= left) left = last[ch] + 1;
        last[ch] = right;
        const len = right - left + 1;
        if (len > best) best = len;
    }
    return best;
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const raw = lines.next() orelse "";
        const s = std.mem.trim(u8, raw, "\r");
        var line_buf: [32]u8 = undefined;
        const line = try std.fmt.bufPrint(&line_buf, "{d}", .{longest(s)});
        _ = c.write(1, line.ptr, line.len);
        if (i + 1 < t) _ = c.write(1, "\n", 1);
    }
}
