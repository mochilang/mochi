const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn expand(s: []const u8, left0: i64, right0: i64) struct { start: usize, len: usize } {
    var left = left0;
    var right = right0;
    while (left >= 0 and right < s.len and s[@intCast(left)] == s[@intCast(right)]) {
        left -= 1;
        right += 1;
    }
    return .{ .start = @intCast(left + 1), .len = @intCast(right - left - 1) };
}

fn longestPalindrome(s: []const u8) []const u8 {
    var best_start: usize = 0;
    var best_len: usize = if (s.len > 0) 1 else 0;
    for (0..s.len) |i| {
        const odd = expand(s, @intCast(i), @intCast(i));
        if (odd.len > best_len) {
            best_start = odd.start;
            best_len = odd.len;
        }
        const even = expand(s, @intCast(i), @intCast(i + 1));
        if (even.len > best_len) {
            best_start = even.start;
            best_len = even.len;
        }
    }
    return s[best_start .. best_start + best_len];
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
        const ans = longestPalindrome(s);
        _ = c.write(1, ans.ptr, ans.len);
        if (i + 1 < t) _ = c.write(1, "\n", 1);
    }
}
