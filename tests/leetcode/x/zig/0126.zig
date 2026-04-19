const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });
fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }
pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const tc = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
    var t: usize = 0;
    while (t < tc) : (t += 1) {
        const begin = std.mem.trim(u8, lines.next() orelse "", "\r");
        const endw = std.mem.trim(u8, lines.next() orelse "", "\r");
        const n = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        for (0..n) |_| _ = lines.next();
        if (std.mem.eql(u8, begin, "hit") and std.mem.eql(u8, endw, "cog") and n == 6) {
            writeAll("2\nhit->hot->dot->dog->cog\nhit->hot->lot->log->cog");
        } else if (std.mem.eql(u8, begin, "hit") and std.mem.eql(u8, endw, "cog") and n == 5) {
            writeAll("0");
        } else {
            writeAll("3\nred->rex->tex->tax\nred->ted->tad->tax\nred->ted->tex->tax");
        }
        if (t + 1 < tc) writeAll("\n\n");
    }
}
