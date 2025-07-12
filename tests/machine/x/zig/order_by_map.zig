const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

const DataItem = struct {
    a: i32,
    b: i32,
};
const data = &[_]DataItem{
    DataItem{
    .a = 1,
    .b = 2,
},
    DataItem{
    .a = 1,
    .b = 1,
},
    DataItem{
    .a = 0,
    .b = 5,
},
}; // []const Dataitem
const sorted = blk0: { var _tmp0 = std.ArrayList(struct { item: DataItem, key: struct {
    a: i32,
    b: i32,
} }).init(std.heap.page_allocator); for (data) |x| { _tmp0.append(.{ .item = x, .key = Dataitem{
    .a = x.a,
    .b = x.b,
} }) catch |err| handleError(err); } for (0.._tmp0.items.len) |i| { for (i+1.._tmp0.items.len) |j| { if (_tmp0.items[j].key < _tmp0.items[i].key) { const t = _tmp0.items[i]; _tmp0.items[i] = _tmp0.items[j]; _tmp0.items[j] = t; } } } var _tmp1 = std.ArrayList(DataItem).init(std.heap.page_allocator);for (_tmp0.items) |p| { _tmp1.append(p.item) catch |err| handleError(err); } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp2; }; // []const DataItem

pub fn main() void {
    std.debug.print("{any}\n", .{sorted});
}
