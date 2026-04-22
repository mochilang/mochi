const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn maxProfit(prices: []const i32) i32 {
    var buy1: i32 = -1000000000;
    var sell1: i32 = 0;
    var buy2: i32 = -1000000000;
    var sell2: i32 = 0;
    for (prices) |p| {
        buy1 = @max(buy1, -p);
        sell1 = @max(sell1, buy1 + p);
        buy2 = @max(buy2, sell1 - p);
        sell2 = @max(sell2, buy2 + p);
    }
    return sell2;
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
        const n_raw = lines.next() orelse "0";
        const n = try std.fmt.parseInt(usize, std.mem.trim(u8, n_raw, " \r\t"), 10);
        var prices: [100005]i32 = undefined;
        for (0..n) |i| {
            const raw = lines.next() orelse "0";
            prices[i] = try std.fmt.parseInt(i32, std.mem.trim(u8, raw, " \r\t"), 10);
        }
        var outbuf: [32]u8 = undefined;
        const out = try std.fmt.bufPrint(&outbuf, "{d}", .{maxProfit(prices[0..n])});
        _ = c.write(1, out.ptr, out.len);
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
