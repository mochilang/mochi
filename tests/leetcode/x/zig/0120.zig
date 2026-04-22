const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = std.mem.trim(u8, lines.next() orelse return, " \r\t");
    const t = try std.fmt.parseInt(usize, first, 10);

    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const rows_line = std.mem.trim(u8, lines.next() orelse return, " \r\t");
        const rows = try std.fmt.parseInt(usize, rows_line, 10);

        var tri = try std.heap.page_allocator.alloc([]i128, rows);
        defer std.heap.page_allocator.free(tri);

        for (0..rows) |r| {
            tri[r] = try std.heap.page_allocator.alloc(i128, r + 1);
            for (0..(r + 1)) |j| {
                const line = std.mem.trim(u8, lines.next() orelse return, " \r\t");
                tri[r][j] = try std.fmt.parseInt(i128, line, 10);
            }
        }

        var dp = try std.heap.page_allocator.alloc(i128, rows);
        defer std.heap.page_allocator.free(dp);
        for (0..rows) |j| dp[j] = tri[rows - 1][j];

        if (rows > 1) {
            var next = try std.heap.page_allocator.alloc(i128, rows);
            defer std.heap.page_allocator.free(next);

            var r: usize = rows - 1;
            while (r > 0) {
                r -= 1;
                for (0..(r + 1)) |j| {
                    next[j] = tri[r][j] + @min(dp[j], dp[j + 1]);
                }
                for (0..(r + 1)) |j| dp[j] = next[j];
            }
        }

        if (tc > 0) writeAll("\n");
        var outbuf: [32]u8 = undefined;
        const out = try std.fmt.bufPrint(&outbuf, "{}", .{dp[0]});
        writeAll(out);

        for (0..rows) |r| {
            std.heap.page_allocator.free(tri[r]);
        }
    }
}
