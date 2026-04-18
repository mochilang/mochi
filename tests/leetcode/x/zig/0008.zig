const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn myAtoi(s: []const u8) i32 {
    var i: usize = 0;
    while (i < s.len and s[i] == ' ') : (i += 1) {}
    var sign: i32 = 1;
    if (i < s.len and (s[i] == '+' or s[i] == '-')) {
        if (s[i] == '-') sign = -1;
        i += 1;
    }
    var ans: i32 = 0;
    const limit: i32 = if (sign > 0) 7 else 8;
    while (i < s.len and s[i] >= '0' and s[i] <= '9') : (i += 1) {
        const digit: i32 = s[i] - '0';
        if (ans > 214748364 or (ans == 214748364 and digit > limit)) return if (sign > 0) 2147483647 else -2147483648;
        ans = ans * 10 + digit;
    }
    return sign * ans;
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
        const line = try std.fmt.bufPrint(&line_buf, "{d}", .{myAtoi(s)});
        _ = c.write(1, line.ptr, line.len);
        if (i + 1 < t) _ = c.write(1, "\n", 1);
    }
}
