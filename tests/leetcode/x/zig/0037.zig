const std = @import("std"); const c = @cImport({ @cInclude("unistd.h"); });
fn valid(b: *[9][9]u8, r: usize, ccol: usize, ch: u8) bool {
    for (0..9) |i| {
        if (b[r][i] == ch or b[i][ccol] == ch) return false;
    }
    const br = (r / 3) * 3;
    const bc = (ccol / 3) * 3;
    for (br..br + 3) |i| {
        for (bc..bc + 3) |j| {
            if (b[i][j] == ch) return false;
        }
    }
    return true;
}
fn solve(b: *[9][9]u8) bool { for (0..9) |r| { for (0..9) |c2| { if (b[r][c2] == '.') { var ch: u8 = '1'; while (ch <= '9') : (ch += 1) { if (valid(b, r, c2, ch)) { b[r][c2] = ch; if (solve(b)) return true; b[r][c2] = '.'; } } return false; } } } return true; }
pub fn main() !void { var buf: [1 << 20]u8 = undefined; const read_n = c.read(0, &buf, buf.len); if (read_n <= 0) return; const input = buf[0..@intCast(read_n)]; var lines = std.mem.splitScalar(u8, input, '\n'); const first = lines.next() orelse return; const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10); var tc: usize = 0; while (tc < t) : (tc += 1) { var b: [9][9]u8 = undefined; for (0..9) |i| { const s = std.mem.trim(u8, lines.next() orelse "", "\r"); for (0..9) |j| b[i][j] = if (j < s.len) s[j] else '.'; } _ = solve(&b); for (0..9) |i| { _ = c.write(1, &b[i], 9); if (tc + 1 < t or i < 8) _ = c.write(1, "\n", 1); } } }
