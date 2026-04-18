const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });
fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }
fn spaces(n: usize, buf: []u8) []u8 { @memset(buf[0..n], ' '); return buf[0..n]; }
pub fn main() !void {
    var buf: [1 << 20]u8 = undefined; const rn = c.read(0, &buf, buf.len); if (rn <= 0) return;
    const input = buf[0..@intCast(rn)]; var lines = std.mem.splitScalar(u8, input, '\n'); const first = lines.next() orelse return; const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    const alloc = std.heap.page_allocator; var tc: usize = 0; while (tc < t) : (tc += 1) {
        const n = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        const words = try alloc.alloc([]const u8, n); for (0..n) |i| words[i] = std.mem.trim(u8, lines.next() orelse "", "\r");
        const width = try std.fmt.parseInt(usize, std.mem.trim(u8, lines.next() orelse "0", " \r\t"), 10);
        var out_count: usize = 0; var i: usize = 0; while (i < n) { var j = i; var total: usize = 0; while (j < n and total + words[j].len + (j - i) <= width) : (j += 1) total += words[j].len; out_count += 1; i = j; }
        var numbuf: [32]u8 = undefined; const cnt = try std.fmt.bufPrint(&numbuf, "{}", .{out_count}); writeAll(cnt); writeAll("\n");
        i = 0; while (i < n) {
            var j = i; var total: usize = 0; while (j < n and total + words[j].len + (j - i) <= width) : (j += 1) total += words[j].len;
            writeAll("|");
            const gaps = j - i - 1; var spbuf: [128]u8 = undefined;
            if (j == n or gaps == 0) {
                var k = i; var len: usize = 0; while (k < j) : (k += 1) { if (k > i) { writeAll(" "); len += 1; } writeAll(words[k]); len += words[k].len; }
                writeAll(spaces(width - len, &spbuf));
            } else {
                const space_total = width - total; const base = space_total / gaps; const extra = space_total % gaps;
                var k = i; while (k < j - 1) : (k += 1) { const plus: usize = if (k - i < extra) 1 else 0; writeAll(words[k]); writeAll(spaces(base + plus, &spbuf)); }
                writeAll(words[j - 1]);
            }
            writeAll("|\n"); i = j;
        }
        if (tc + 1 < t) writeAll("=\n");
    }
}
