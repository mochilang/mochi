const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const DataItem = struct {
    tag: []const u8,
    val: i32,
};
const data = &[_]DataItem{
    DataItem{
    .tag = "a",
    .val = 1,
},
    DataItem{
    .tag = "a",
    .val = 2,
},
    DataItem{
    .tag = "b",
    .val = 3,
},
}; // []const DataItem
const groups = blk0: { var _tmp0 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(DataItem) }).init(std.heap.page_allocator); var _tmp1 = std.StringHashMap(usize).init(std.heap.page_allocator); for (data) |d| { const _tmp2 = d.tag; if (_tmp1.get(_tmp2)) |idx| { _tmp0.items[idx].Items.append(d) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(DataItem) }{ .key = _tmp2, .Items = std.ArrayList(DataItem).init(std.heap.page_allocator) }; g.Items.append(d) catch unreachable; _tmp0.append(g) catch unreachable; _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable; } } var _tmp3 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(DataItem) }).init(std.heap.page_allocator);for (_tmp0.items) |g| { _tmp3.append(g) catch unreachable; } var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp3.items) |g| { _tmp4.append(g) catch unreachable; } const _tmp4Slice = _tmp4.toOwnedSlice() catch unreachable; break :blk0 _tmp4Slice; }; // []const i32
var tmp = &[]i32{}; // []const i32
const result = blk1: { var _tmp5 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator); for (tmp) |r| { _tmp5.append(.{ .item = r, .key = r.tag }) catch unreachable; } for (0.._tmp5.items.len) |i| { for (i+1.._tmp5.items.len) |j| { if (_tmp5.items[j].key < _tmp5.items[i].key) { const t = _tmp5.items[i]; _tmp5.items[i] = _tmp5.items[j]; _tmp5.items[j] = t; } } } var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp5.items) |p| { _tmp6.append(p.item) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk1 _tmp7; }; // []const i32

pub fn main() void {
    for (groups) |g| {
        var total = 0; // i32
        for (g.items) |x| {
            total = (total + x.val);
        }
        tmp = blk2: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp8.deinit(); _tmp8.appendSlice(tmp) catch unreachable; _tmp8.append(struct {
    tag: []const u8,
    total: i32,
}{
    .tag = g.key,
    .total = total,
}) catch unreachable; break :blk2 _tmp8.items; };
    }
    std.debug.print("{any}\n", .{result});
}
