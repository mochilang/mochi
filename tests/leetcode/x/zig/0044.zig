const std = @import("std"); const c = @cImport({ @cInclude("unistd.h"); });
fn isMatch(s: []const u8, p: []const u8) bool {
    var i: usize = 0; var j: usize = 0; var star: ?usize = null; var mt: usize = 0;
    while (i < s.len) {
        if (j < p.len and (p[j] == '?' or p[j] == s[i])) { i += 1; j += 1; }
        else if (j < p.len and p[j] == '*') { star = j; mt = i; j += 1; }
        else if (star != null) { j = star.? + 1; mt += 1; i = mt; }
        else return false;
    }
    while (j < p.len and p[j] == '*') j += 1;
    return j == p.len;
}
pub fn main() !void {
    var buf: [1 << 20]u8 = undefined; const read_n = c.read(0, &buf, buf.len); if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)]; var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return; const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var tc: usize = 0; while (tc < t) : (tc += 1) {
        const nline = std.mem.trim(u8, lines.next() orelse "0", " \r\t"); const n = try std.fmt.parseInt(usize, nline, 10);
        const s = if (n > 0) std.mem.trim(u8, lines.next() orelse "", "\r") else "";
        const mline = std.mem.trim(u8, lines.next() orelse "0", " \r\t"); const m = try std.fmt.parseInt(usize, mline, 10);
        const p = if (m > 0) std.mem.trim(u8, lines.next() orelse "", "\r") else "";
        if (tc > 0) _ = c.write(1, "\n", 1);
        const ans = if (isMatch(s, p)) "true" else "false"; _ = c.write(1, ans.ptr, ans.len);
    }
}
