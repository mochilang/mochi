const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });
fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }
fn isDigit(ch: u8) bool { return ch >= '0' and ch <= '9'; }
fn isNumber(s: []const u8) bool {
    var seen_digit = false;
    var seen_dot = false;
    var seen_exp = false;
    var digit_after_exp = true;
    for (s, 0..) |ch, i| {
        if (isDigit(ch)) {
            seen_digit = true;
            if (seen_exp) digit_after_exp = true;
        } else if (ch == '+' or ch == '-') {
            if (i != 0 and s[i - 1] != 'e' and s[i - 1] != 'E') return false;
        } else if (ch == '.') {
            if (seen_dot or seen_exp) return false;
            seen_dot = true;
        } else if (ch == 'e' or ch == 'E') {
            if (seen_exp or !seen_digit) return false;
            seen_exp = true;
            digit_after_exp = false;
        } else return false;
    }
    return seen_digit and digit_after_exp;
}
pub fn main() !void {
    var buf:[1<<20]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const s = std.mem.trim(u8, lines.next() orelse "", "\r");
        writeAll(if (isNumber(s)) "true" else "false");
        if (i + 1 < t) writeAll("\n");
    }
}
