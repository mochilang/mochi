const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn value(ch: u8) i64 {
    return switch (ch) {
        'I' => 1, 'V' => 5, 'X' => 10, 'L' => 50, 'C' => 100, 'D' => 500, else => 1000,
    };
}

fn romanToInt(s: []const u8) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        const cur = value(s[i]);
        const next = if (i + 1 < s.len) value(s[i + 1]) else 0;
        total += if (cur < next) -cur else cur;
    }
    return total;
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var tokens = std.mem.tokenizeAny(u8, input, " \n\r\t");
    const t = try std.fmt.parseInt(usize, tokens.next().?, 10);
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const s = tokens.next().?;
        var line_buf: [32]u8 = undefined;
        const line = if (i + 1 < t)
            try std.fmt.bufPrint(&line_buf, "{d}\n", .{romanToInt(s)})
        else
            try std.fmt.bufPrint(&line_buf, "{d}", .{romanToInt(s)});
        _ = c.write(1, line.ptr, line.len);
    }
}
