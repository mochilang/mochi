const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn firstMissingPositive(nums: []i32) i32 {
    const n = nums.len;
    var i: usize = 0;
    while (i < n) {
        const v = nums[i];
        if (v >= 1 and v <= n and nums[@intCast(v - 1)] != v) {
            const j: usize = @intCast(v - 1);
            const tmp = nums[i];
            nums[i] = nums[j];
            nums[j] = tmp;
        } else {
            i += 1;
        }
    }
    i = 0;
    while (i < n) : (i += 1) {
        if (nums[i] != i + 1) return @intCast(i + 1);
    }
    return @intCast(n + 1);
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
        const nline = std.mem.trim(u8, lines.next() orelse "0", " \r\t");
        const n = try std.fmt.parseInt(usize, nline, 10);
        var nums = try std.heap.page_allocator.alloc(i32, n);
        defer std.heap.page_allocator.free(nums);
        for (0..n) |k| {
            const s = std.mem.trim(u8, lines.next() orelse "0", " \r\t");
            nums[k] = try std.fmt.parseInt(i32, s, 10);
        }
        if (tc > 0) _ = c.write(1, "\n", 1);
        var out: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&out, "{}", .{firstMissingPositive(nums)});
        _ = c.write(1, text.ptr, text.len);
    }
}
