const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void {
    _ = c.write(1, bytes.ptr, bytes.len);
}

fn solve(a: []const i64) i64 {
    var best: i64 = 0;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        var mn = a[i];
        var j: usize = i;
        while (j < a.len) : (j += 1) {
            if (a[j] < mn) mn = a[j];
            const width: i64 = @intCast(j - i + 1);
            const area = mn * width;
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
    var it = std.mem.tokenizeAny(u8, input, " \n\r\t");
    const t_str = it.next() orelse return;
    const t = try std.fmt.parseInt(usize, t_str, 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const n = try std.fmt.parseInt(usize, it.next().?, 10);
        var arr = try std.heap.page_allocator.alloc(i64, n);
        defer std.heap.page_allocator.free(arr);
        for (0..n) |i| arr[i] = try std.fmt.parseInt(i64, it.next().?, 10);
        if (tc > 0) writeAll("\n");
        var numbuf: [32]u8 = undefined;
        const s = try std.fmt.bufPrint(&numbuf, "{}", .{solve(arr)});
        writeAll(s);
    }
}
