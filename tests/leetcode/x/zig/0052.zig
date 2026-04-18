const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

fn writeInt(value: usize) !void {
    var buf: [32]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, "{}", .{value});
    writeAll(text);
}

fn countDfs(r: usize, n: usize, cols: []bool, d1: []bool, d2: []bool) usize {
    if (r == n) return 1;
    var total: usize = 0;
    for (0..n) |cc| {
        const a = r + cc;
        const b = r + (n - 1 - cc);
        if (cols[cc] or d1[a] or d2[b]) continue;
        cols[cc] = true;
        d1[a] = true;
        d2[b] = true;
        total += countDfs(r + 1, n, cols, d1, d2);
        cols[cc] = false;
        d1[a] = false;
        d2[b] = false;
    }
    return total;
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first_line = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first_line, " \r\t"), 10);

    const alloc = std.heap.page_allocator;
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const n_line = std.mem.trim(u8, lines.next() orelse "0", " \r\t");
        const n = try std.fmt.parseInt(usize, n_line, 10);
        const cols = try alloc.alloc(bool, n);
        const d1 = try alloc.alloc(bool, 2 * n);
        const d2 = try alloc.alloc(bool, 2 * n);
        @memset(cols, false);
        @memset(d1, false);
        @memset(d2, false);
        try writeInt(countDfs(0, n, cols, d1, d2));
        if (tc + 1 < t) writeAll("\n");
    }
}
