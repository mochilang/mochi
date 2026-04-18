const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });
fn less(_: void, a: []const u8, b: []const u8) bool { return std.mem.order(u8, a, b) == .lt; }
pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len); if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return; const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var tc: usize = 0;
    while (tc < t) : (tc += 1) {
        const s = std.mem.trim(u8, lines.next() orelse "", " \r\t");
        const m = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var words: [128][]const u8 = undefined;
        for (0..m) |i| words[i] = std.mem.trim(u8, lines.next() orelse "", " \r\t");
        if (m == 0) { _ = c.write(1, "[]", 2); if (tc + 1 < t) _ = c.write(1, "\n", 1); continue; }
        const wlen = words[0].len; const total = wlen * m;
        std.mem.sort([]const u8, words[0..m], {}, less);
        _ = c.write(1, "[", 1);
        var first_out = true;
        var i: usize = 0;
        while (i + total <= s.len) : (i += 1) {
            var parts: [128][]const u8 = undefined;
            for (0..m) |j| parts[j] = s[i + j*wlen .. i + (j+1)*wlen];
            std.mem.sort([]const u8, parts[0..m], {}, less);
            var ok = true; for (0..m) |j| { if (!std.mem.eql(u8, parts[j], words[j])) { ok = false; break; } }
            if (ok) { if (!first_out) _ = c.write(1, ",", 1); first_out = false; var num_buf: [32]u8 = undefined; const num = try std.fmt.bufPrint(&num_buf, "{d}", .{i}); _ = c.write(1, num.ptr, num.len); }
        }
        _ = c.write(1, "]", 1); if (tc + 1 < t) _ = c.write(1, "\n", 1);
    }
}
