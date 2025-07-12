const std = @import("std");

const ItemsItem = struct {
    n: i32,
    v: []const u8,
};
const items = &[_]ItemsItem{
    ItemsItem{
    .n = 1,
    .v = "a",
},
    ItemsItem{
    .n = 1,
    .v = "b",
},
    ItemsItem{
    .n = 2,
    .v = "c",
},
}; // []const ItemsItem
const result = blk0: { var _tmp0 = std.ArrayList(struct { item: u8, key: i32 }).init(std.heap.page_allocator); for (items) |i| { _tmp0.append(.{ .item = i.v, .key = i.n }) catch unreachable; } for (0.._tmp0.items.len) |i| { for (i+1.._tmp0.items.len) |j| { if (_tmp0.items[j].key < _tmp0.items[i].key) { const t = _tmp0.items[i]; _tmp0.items[i] = _tmp0.items[j]; _tmp0.items[j] = t; } } } var _tmp1 = std.ArrayList(u8).init(std.heap.page_allocator);for (_tmp0.items) |p| { _tmp1.append(p.item) catch unreachable; } const _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk0 _tmp2; }; // []const []const u8

pub fn main() void {
    std.debug.print("{any}\n", .{result});
}
