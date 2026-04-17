const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn startsWith(s: []const u8, p: []const u8) bool {
    return p.len <= s.len and std.mem.eql(u8, s[0..p.len], p);
}

fn lcp(strs: []const []const u8) []const u8 {
    var prefix = strs[0];
    while (true) {
        var ok = true;
        for (strs) |s| {
            if (!startsWith(s, prefix)) {
                ok = false;
            }
        }
        if (ok) return prefix;
        prefix = prefix[0 .. prefix.len - 1];
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var tokens = std.mem.tokenizeAny(u8, input, " \n\r\t");
    const t = try std.fmt.parseInt(usize, tokens.next().?, 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const n = try std.fmt.parseInt(usize, tokens.next().?, 10);
        const strs = try allocator.alloc([]const u8, n);
        for (0..n) |i| strs[i] = tokens.next().?;
        var line_buf: [256]u8 = undefined;
        const line = if (tc + 1 < t)
            try std.fmt.bufPrint(&line_buf, "\"{s}\"\n", .{lcp(strs)})
        else
            try std.fmt.bufPrint(&line_buf, "\"{s}\"", .{lcp(strs)});
        _ = c.write(1, line.ptr, line.len);
    }
}
