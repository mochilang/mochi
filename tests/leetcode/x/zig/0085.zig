const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

fn hist(h: []const i64) i64 {
    var best: i64 = 0;
    var i: usize = 0;
    while (i < h.len) : (i += 1) {
        var mn = h[i];
        var j: usize = i;
        while (j < h.len) : (j += 1) {
            if (h[j] < mn) mn = h[j];
            const area = mn * @as(i64, @intCast(j - i + 1));
            if (area > best) best = area;
        }
    }
    return best;
}

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
        const rc = std.mem.trim(u8, lines.next() orelse "", " \r\t");
        var it = std.mem.tokenizeAny(u8, rc, " ");
        const rows = try std.fmt.parseInt(usize, it.next().?, 10);
        const cols = try std.fmt.parseInt(usize, it.next().?, 10);
        var h = try std.heap.page_allocator.alloc(i64, cols);
        defer std.heap.page_allocator.free(h);
        for (0..cols) |i| h[i] = 0;
        var best: i64 = 0;
        var r: usize = 0;
        while (r < rows) : (r += 1) {
            const s = std.mem.trim(u8, lines.next() orelse "", "\r");
            for (0..cols) |cidx| h[cidx] = if (s[cidx] == '1') h[cidx] + 1 else 0;
            const area = hist(h);
            if (area > best) best = area;
        }
        if (tc > 0) writeAll("\n");
        var numbuf: [32]u8 = undefined;
        const out = try std.fmt.bufPrint(&numbuf, "{}", .{best});
        writeAll(out);
    }
}
