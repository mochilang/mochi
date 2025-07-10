const std = @import("std");

fn _print_list(comptime T: type, v: []const T) void {
    for (v, 0..) |it, i| {
        if (i > 0) std.debug.print(" ", .{});
        std.debug.print("{any}", .{it});
    }
    std.debug.print("\n", .{});
}

const items = (blk0: { const _tmp0 = struct {
    n: i32,
    v: []const u8,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .n = 1,
    .v = "a",
},
    _tmp0{
    .n = 1,
    .v = "b",
},
    _tmp0{
    .n = 2,
    .v = "c",
},
}; break :blk0 _arr; });
const result = blk1: { var _tmp1 = std.ArrayList(struct { item: u8, key: i32 }).init(std.heap.page_allocator); for (items) |i| { _tmp1.append(.{ .item = i.v, .key = i.n }) catch unreachable; } for (0.._tmp1.items.len) |i| { for (i+1.._tmp1.items.len) |j| { if (_tmp1.items[j].key < _tmp1.items[i].key) { const t = _tmp1.items[i]; _tmp1.items[i] = _tmp1.items[j]; _tmp1.items[j] = t; } } } var _tmp2 = std.ArrayList(u8).init(std.heap.page_allocator);for (_tmp1.items) |p| { _tmp2.append(p.item) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk1 _tmp3; };

pub fn main() void {
    _print_list([]const u8, result);
}
