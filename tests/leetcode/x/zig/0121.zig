const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn maxProfit(prices: []const i32) i32 {
    if (prices.len == 0) return 0;
    var min_price = prices[0];
    var best: i32 = 0;
    for (prices[1..]) |p| {
        if (p - min_price > best) best = p - min_price;
        if (p < min_price) min_price = p;
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
