const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn twoSum(nums: []const i64, target: i64) [2]usize {
    var i: usize = 0;
    while (i < nums.len) : (i += 1) {
        var j: usize = i + 1;
        while (j < nums.len) : (j += 1) {
            if (nums[i] + nums[j] == target) {
                return .{ i, j };
            }
        }
    }
    return .{ 0, 0 };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) {
        return;
    }
    const input = buf[0..@intCast(read_n)];
    if (std.mem.trim(u8, input, " \n\r\t").len == 0) {
        return;
    }

    var tokens = std.mem.tokenizeAny(u8, input, " \n\r\t");
    const t = try std.fmt.parseInt(usize, tokens.next().?, 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const n = try std.fmt.parseInt(usize, tokens.next().?, 10);
        const target = try std.fmt.parseInt(i64, tokens.next().?, 10);
        const nums = try allocator.alloc(i64, n);
        var i: usize = 0;
        while (i < n) : (i += 1) {
            nums[i] = try std.fmt.parseInt(i64, tokens.next().?, 10);
        }
        const ans = twoSum(nums, target);
        var line_buf: [32]u8 = undefined;
        const line = if (tc + 1 < t)
            try std.fmt.bufPrint(&line_buf, "{d} {d}\n", .{ ans[0], ans[1] })
        else
            try std.fmt.bufPrint(&line_buf, "{d} {d}", .{ ans[0], ans[1] });
        _ = c.write(1, line.ptr, line.len);
    }
}
