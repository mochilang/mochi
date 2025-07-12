const std = @import("std");

fn _slice_string(s: []const u8, start: i32, end: i32, step: i32) []const u8 {
    var sidx = start;
    var eidx = end;
    var stp = step;
    const n: i32 = @as(i32, @intCast(s.len));
    if (sidx < 0) sidx += n;
    if (eidx < 0) eidx += n;
    if (stp == 0) stp = 1;
    if (sidx < 0) sidx = 0;
    if (eidx > n) eidx = n;
    if (stp > 0 and eidx < sidx) eidx = sidx;
    if (stp < 0 and eidx > sidx) eidx = sidx;
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = sidx;
    while ((stp > 0 and i < eidx) or (stp < 0 and i > eidx)) : (i += stp) {
        res.append(s[@as(usize, @intCast(i))]) catch |err| handleError(err);
    }
    return res.toOwnedSlice() catch |err| handleError(err);
}

const prefix = "fore"; // []const u8
const s1 = "forest"; // []const u8
const s2 = "desert"; // []const u8

pub fn main() void {
    std.debug.print("{}\n", .{std.mem.eql(u8, _slice_string(s1, 0, (prefix).len, 1), prefix)});
    std.debug.print("{}\n", .{std.mem.eql(u8, _slice_string(s2, 0, (prefix).len, 1), prefix)});
}
