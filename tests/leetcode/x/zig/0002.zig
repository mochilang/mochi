const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn format(buf: []u8, arr: []const i64) ![]const u8 {
    var pos: usize = 0;
    buf[pos] = '[';
    pos += 1;
    for (arr, 0..) |v, i| {
        if (i > 0) {
            buf[pos] = ',';
            pos += 1;
        }
        const written = try std.fmt.bufPrint(buf[pos..], "{d}", .{v});
        pos += written.len;
    }
    buf[pos] = ']';
    pos += 1;
    return buf[0..pos];
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var tokens = std.mem.tokenizeAny(u8, input, " \n\r\t");
    const t = try std.fmt.parseInt(usize, tokens.next().?, 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const n = try std.fmt.parseInt(usize, tokens.next().?, 10);
        const a = try alloc.alloc(i64, n);
        for (0..n) |i| a[i] = try std.fmt.parseInt(i64, tokens.next().?, 10);
        const m = try std.fmt.parseInt(usize, tokens.next().?, 10);
        const b = try alloc.alloc(i64, m);
        for (0..m) |i| b[i] = try std.fmt.parseInt(i64, tokens.next().?, 10);
        const out = try alloc.alloc(i64, n + m + 1);
        var i: usize = 0; var j: usize = 0; var k: usize = 0; var carry: i64 = 0;
        while (i < n or j < m or carry > 0) {
            var sum = carry;
            if (i < n) { sum += a[i]; i += 1; }
            if (j < m) { sum += b[j]; j += 1; }
            out[k] = @mod(sum, 10);
            k += 1;
            carry = @divTrunc(sum, 10);
        }
        var line_buf: [256]u8 = undefined;
        const line = try format(&line_buf, out[0..k]);
        _ = c.write(1, line.ptr, line.len);
        if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
