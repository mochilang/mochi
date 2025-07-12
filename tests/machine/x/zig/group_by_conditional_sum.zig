const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const ItemsItem = struct {
    cat: []const u8,
    val: i32,
    flag: bool,
};
const items = &[_]ItemsItem{
    ItemsItem{
    .cat = "a",
    .val = 10,
    .flag = true,
},
    ItemsItem{
    .cat = "a",
    .val = 5,
    .flag = false,
},
    ItemsItem{
    .cat = "b",
    .val = 20,
    .flag = true,
},
}; // []const ItemsItem
const result = blk2: { var _tmp4 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator); var _tmp5 = std.StringHashMap(usize).init(std.heap.page_allocator); for (items) |i| { const _tmp6 = i.cat; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(i) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(ItemsItem) }{ .key = _tmp6, .Items = std.ArrayList(ItemsItem).init(std.heap.page_allocator) }; g.Items.append(i) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } var _tmp7 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(g) catch unreachable; } var _tmp8 = std.ArrayList(struct { item: struct { key: []const u8, Items: std.ArrayList(ItemsItem) }, key: i32 }).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp8.append(.{ .item = g, .key = g.key }) catch unreachable; } for (0.._tmp8.items.len) |i| { for (i+1.._tmp8.items.len) |j| { if (_tmp8.items[j].key < _tmp8.items[i].key) { const t = _tmp8.items[i]; _tmp8.items[i] = _tmp8.items[j]; _tmp8.items[j] = t; } } } var _tmp9 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator);for (_tmp8.items) |p| { _tmp9.append(p.item) catch unreachable; } var _tmp10 = std.ArrayList(struct {
    cat: i32,
    share: f64,
}).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp10.append(struct {
    cat: i32,
    share: f64,
}{
    .cat = g.key,
    .share = (_sum_int(blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(if (x.flag) (x.val) else (0)) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; }) / _sum_int(blk1: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.val) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk1 _tmp3; })),
}) catch unreachable; } const _tmp10Slice = _tmp10.toOwnedSlice() catch unreachable; break :blk2 _tmp10Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("{any}\n", .{result});
}
