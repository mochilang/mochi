const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn maxArea(h: []const i32) i32 {
    var left: usize = 0;
    var right: usize = h.len - 1;
    var best: i32 = 0;
    while (left < right) {
        const height = if (h[left] < h[right]) h[left] else h[right];
        const area = @as(i32, @intCast(right - left)) * height;
        if (area > best) best = area;
        if (h[left] < h[right]) left += 1 else right -= 1;
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
        var vals: [100005]i32 = undefined;
        for (0..n) |i| {
            const raw = lines.next() orelse "0";
            vals[i] = try std.fmt.parseInt(i32, std.mem.trim(u8, raw, " \r\t"), 10);
        }
        var line_buf: [32]u8 = undefined;
        const line = try std.fmt.bufPrint(&line_buf, "{d}", .{maxArea(vals[0..n])});
        _ = c.write(1, line.ptr, line.len);
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
