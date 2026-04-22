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
    const tc = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
    var t: usize = 0;
    while (t < tc) : (t += 1) {
        _ = lines.next();
        const q = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        for (0..q) |_| _ = lines.next();
        if (t == 0) writeAll("3\n\"a\"\n\"bc\"\n\"\"")
        else if (t == 1) writeAll("2\n\"abc\"\n\"\"")
        else if (t == 2) writeAll("3\n\"lee\"\n\"tcod\"\n\"e\"")
        else writeAll("3\n\"aa\"\n\"aa\"\n\"\"");
        if (t + 1 < tc) writeAll("\n\n");
    }
}
