const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn reverseInt(x0: i32) i32 {
    var x = x0;
    var ans: i32 = 0;
    while (x != 0) {
        const digit = @rem(x, 10);
        x = @divTrunc(x, 10);
        if (ans > 214748364 or (ans == 214748364 and digit > 7)) return 0;
        if (ans < -214748364 or (ans == -214748364 and digit < -8)) return 0;
        ans = ans * 10 + digit;
    }
    return ans;
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const raw = lines.next() orelse "0";
        const x = try std.fmt.parseInt(i32, std.mem.trim(u8, raw, " \r\t"), 10);
        var line_buf: [32]u8 = undefined;
        const line = try std.fmt.bufPrint(&line_buf, "{d}", .{reverseInt(x)});
        _ = c.write(1, line.ptr, line.len);
        if (i + 1 < t) _ = c.write(1, "\n", 1);
    }
}
