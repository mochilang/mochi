const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const n = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var k = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var digits: [9]u8 = undefined;
        var fact: [10]usize = undefined;
        fact[0] = 1;
        for (0..n) |i| digits[i] = @as(u8, '1') + @as(u8, @intCast(i));
        for (1..(n + 1)) |i| fact[i] = fact[i - 1] * i;
        k -= 1;
        var len = n;
        var out: [16]u8 = undefined;
        for (0..n) |pos| {
            const rem = n - pos;
            const block = fact[rem - 1];
            const idx = k / block;
            k %= block;
            out[pos] = digits[idx];
            var j = idx;
            while (j + 1 < len) : (j += 1) digits[j] = digits[j + 1];
            len -= 1;
        }
        writeAll(out[0..n]);
        if (tc + 1 < t) writeAll("\n");
    }
}
