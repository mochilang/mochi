const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    var it = std.mem.tokenizeAny(u8, buf[0..@intCast(rn)], " \n\r\t");
    const t = try std.fmt.parseInt(usize, it.next() orelse return, 10);

    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const rows = try std.fmt.parseInt(usize, it.next().?, 10);
        const cols = try std.fmt.parseInt(usize, it.next().?, 10);
        var dungeon = try std.heap.page_allocator.alloc([]i64, rows);
        defer std.heap.page_allocator.free(dungeon);
        for (0..rows) |i| {
            dungeon[i] = try std.heap.page_allocator.alloc(i64, cols);
            for (0..cols) |j| {
                dungeon[i][j] = try std.fmt.parseInt(i64, it.next().?, 10);
            }
        }

        var dp = try std.heap.page_allocator.alloc(i64, cols + 1);
        defer std.heap.page_allocator.free(dp);
        for (0..cols + 1) |j| dp[j] = 1_000_000_000;
        dp[cols - 1] = 1;

        var i: usize = rows;
        while (i > 0) {
            i -= 1;
            var j: usize = cols;
            while (j > 0) {
                j -= 1;
                const best = @min(dp[j], dp[j + 1]);
                const need = best - dungeon[i][j];
                dp[j] = if (need <= 1) 1 else need;
            }
        }

        if (tc > 0) writeAll("\n");
        var outbuf: [32]u8 = undefined;
        const out = try std.fmt.bufPrint(&outbuf, "{}", .{dp[0]});
        writeAll(out);

        for (0..rows) |r| std.heap.page_allocator.free(dungeon[r]);
    }
}
