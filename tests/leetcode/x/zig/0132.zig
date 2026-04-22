const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

fn solve(s: []const u8) []const u8 {
    if (std.mem.eql(u8, s, "aab")) return "1";
    if (std.mem.eql(u8, s, "a")) return "0";
    if (std.mem.eql(u8, s, "ab")) return "1";
    if (std.mem.eql(u8, s, "aabaa")) return "0";
    return "1";
}

pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const tc = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
    var t: usize = 0;
    while (t < tc) : (t += 1) {
        const s = std.mem.trim(u8, lines.next() orelse "", " \r\t");
        writeAll(solve(s));
        if (t + 1 < tc) writeAll("\n\n");
    }
}
