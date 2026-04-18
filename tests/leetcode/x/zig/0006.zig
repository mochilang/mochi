const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn convertZigzag(input: []const u8, num_rows: usize, allocator: std.mem.Allocator) ![]u8 {
    if (num_rows <= 1 or num_rows >= input.len) return allocator.dupe(u8, input);
    const cycle = 2 * num_rows - 2;
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);
    for (0..num_rows) |row| {
        var i = row;
        while (i < input.len) : (i += cycle) {
            try out.append(allocator, input[i]);
            const diag = i + cycle - 2 * row;
            if (row > 0 and row < num_rows - 1 and diag < input.len) try out.append(allocator, input[diag]);
        }
    }
    return out.toOwnedSlice(allocator);
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var lines = std.mem.splitScalar(u8, input, '\n');
    const first = lines.next() orelse return;
    const t = try std.fmt.parseInt(usize, std.mem.trim(u8, first, " \r\t"), 10);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const raw_s = lines.next() orelse "";
        const s = std.mem.trim(u8, raw_s, "\r");
        const raw_r = lines.next() orelse "1";
        const num_rows = try std.fmt.parseInt(usize, std.mem.trim(u8, raw_r, " \r\t"), 10);
        const ans = try convertZigzag(s, num_rows, allocator);
        _ = c.write(1, ans.ptr, ans.len);
        if (i + 1 < t) _ = c.write(1, "\n", 1);
    }
}
