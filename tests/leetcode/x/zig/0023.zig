const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });
fn less(_: void, a: i32, b: i32) bool { return a < b; }
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
        const k = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var vals: [4096]i32 = undefined;
        var len: usize = 0;
        for (0..k) |_| {
            const n = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
            for (0..n) |_| {
                vals[len] = try std.fmt.parseInt(i32, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
                len += 1;
            }
        }
        std.mem.sort(i32, vals[0..len], {}, less);
        _ = c.write(1, "[", 1);
        for (0..len) |i| {
            if (i > 0) _ = c.write(1, ",", 1);
            var num_buf: [32]u8 = undefined;
            const s = try std.fmt.bufPrint(&num_buf, "{d}", .{vals[i]});
            _ = c.write(1, s.ptr, s.len);
        }
        _ = c.write(1, "]", 1);
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
