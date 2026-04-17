const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn median(a: []const i32, b: []const i32) f64 {
    var merged: [4096]i32 = undefined;
    var i: usize = 0;
    var j: usize = 0;
    var k: usize = 0;
    while (i < a.len and j < b.len) {
        if (a[i] <= b[j]) { merged[k] = a[i]; i += 1; } else { merged[k] = b[j]; j += 1; }
        k += 1;
    }
    while (i < a.len) : (i += 1) { merged[k] = a[i]; k += 1; }
    while (j < b.len) : (j += 1) { merged[k] = b[j]; k += 1; }
    if (k % 2 == 1) return @floatFromInt(merged[k / 2]);
    return (@as(f64, @floatFromInt(merged[k / 2 - 1])) + @as(f64, @floatFromInt(merged[k / 2]))) / 2.0;
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
        const n = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var a: [2048]i32 = undefined;
        for (0..n) |idx| a[idx] = try std.fmt.parseInt(i32, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        const m = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var b: [2048]i32 = undefined;
        for (0..m) |idx| b[idx] = try std.fmt.parseInt(i32, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var line_buf: [64]u8 = undefined;
        const line = try std.fmt.bufPrint(&line_buf, "{d:.1}", .{median(a[0..n], b[0..m])});
        _ = c.write(1, line.ptr, line.len);
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
