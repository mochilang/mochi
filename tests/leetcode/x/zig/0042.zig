const std = @import("std"); const c = @cImport({ @cInclude("unistd.h"); });
fn trap(h: []i32) i32 {
    var left: usize = 0;
    var right: usize = h.len - 1;
    var leftMax: i32 = 0;
    var rightMax: i32 = 0;
    var water: i32 = 0;
    while (left <= right) {
        if (leftMax <= rightMax) {
            if (h[left] < leftMax) water += leftMax - h[left] else leftMax = h[left];
            left += 1;
        } else {
            if (h[right] < rightMax) water += rightMax - h[right] else rightMax = h[right];
            if (right == 0) break;
            right -= 1;
        }
    }
    return water;
}
pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len); if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const nline = std.mem.trim(u8, lines.next() orelse "0", " \r\t");
        const n = try std.fmt.parseInt(usize, nline, 10);
        var arr = try std.heap.page_allocator.alloc(i32, n); defer std.heap.page_allocator.free(arr);
        for (0..n) |k| { const s = std.mem.trim(u8, lines.next() orelse "0", " \r\t"); arr[k] = try std.fmt.parseInt(i32, s, 10); }
        if (tc > 0) _ = c.write(1, "\n", 1);
        var out: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&out, "{}", .{trap(arr)});
        _ = c.write(1, text.ptr, text.len);
    }
}
