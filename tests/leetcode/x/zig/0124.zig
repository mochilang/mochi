const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

fn writeAll(bytes: []const u8) void { _ = c.write(1, bytes.ptr, bytes.len); }

fn dfs(i: usize, vals: []const i32, ok: []const bool, best: *i32) i32 {
    if (i >= vals.len or !ok[i]) return 0;
    const left = @max(@as(i32, 0), dfs(2 * i + 1, vals, ok, best));
    const right = @max(@as(i32, 0), dfs(2 * i + 2, vals, ok, best));
    best.* = @max(best.*, vals[i] + left + right);
    return vals[i] + @max(left, right);
}

fn solve(vals: []const i32, ok: []const bool) i32 {
    var best: i32 = -1000000000;
    _ = dfs(0, vals, ok, &best);
    return best;
}

pub fn main() !void {
    var buf: [1 << 16]u8 = undefined;
    const rn = c.read(0, &buf, buf.len);
    if (rn <= 0) return;
    const input = buf[0..@intCast(rn)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const tc = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var t: usize = 0;
    while (t < tc) : (t += 1) {
        const nLine = std.mem.trim(u8, lines.next() orelse "0", " \r\t");
        const n = try std.fmt.parseInt(usize, nLine, 10);
        var vals = try std.heap.page_allocator.alloc(i32, n);
        defer std.heap.page_allocator.free(vals);
        var ok = try std.heap.page_allocator.alloc(bool, n);
        defer std.heap.page_allocator.free(ok);
        for (0..n) |i| {
            const tok = std.mem.trim(u8, lines.next() orelse "null", " \r\t");
            if (std.mem.eql(u8, tok, "null")) {
                vals[i] = 0;
                ok[i] = false;
            } else {
                vals[i] = try std.fmt.parseInt(i32, tok, 10);
                ok[i] = true;
            }
        }
        var outbuf: [32]u8 = undefined;
        const out = try std.fmt.bufPrint(&outbuf, "{}", .{solve(vals, ok)});
        writeAll(out);
        if (t + 1 < tc) writeAll("\n");
    }
}
