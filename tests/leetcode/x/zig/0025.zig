const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });
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
        const n = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var arr: [4096]i32 = undefined;
        for (0..n) |i| arr[i] = try std.fmt.parseInt(i32, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        const k = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "1", " \r\t"), 10);
        var i: usize = 0;
        while (i + k <= n) : (i += k) {
            var l = i;
            var r = i + k - 1;
            while (l < r) : ({ l += 1; r -= 1; }) {
                const tmp = arr[l]; arr[l] = arr[r]; arr[r] = tmp;
            }
        }
        _ = c.write(1, "[", 1);
        for (0..n) |j| {
            if (j > 0) _ = c.write(1, ",", 1);
            var num_buf: [32]u8 = undefined;
            const s = try std.fmt.bufPrint(&num_buf, "{d}", .{arr[j]});
            _ = c.write(1, s.ptr, s.len);
        }
        _ = c.write(1, "]", 1);
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
