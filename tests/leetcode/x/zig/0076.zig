const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

fn minWindow(s: []const u8, t: []const u8, outbuf: []u8) []const u8 {
    var need: [128]i32 = [_]i32{0} ** 128;
    var missing: i32 = @intCast(t.len);
    for (t) |ch| need[ch] += 1;
    var left: usize = 0;
    var best_start: usize = 0;
    var best_len: usize = s.len + 1;
    for (s, 0..) |ch, right| {
        if (need[ch] > 0) missing -= 1;
        need[ch] -= 1;
        while (missing == 0) {
            if (right - left + 1 < best_len) {
                best_start = left;
                best_len = right - left + 1;
            }
            const lc = s[left];
            need[lc] += 1;
            if (need[lc] > 0) missing += 1;
            left += 1;
        }
    }
    if (best_len > s.len) return "";
    @memcpy(outbuf[0..best_len], s[best_start .. best_start + best_len]);
    return outbuf[0..best_len];
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const s = std.mem.trim(u8, lines.next() orelse "", "\r");
        const p = std.mem.trim(u8, lines.next() orelse "", "\r");
        var outbuf: [1 << 16]u8 = undefined;
        const ans = minWindow(s, p, &outbuf);
        writeAll(ans);
        if (i + 1 < t) writeAll("\n");
    }
}
