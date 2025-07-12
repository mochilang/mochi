const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

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
};
const items = &[_]ItemsItem{
    ItemsItem{
    .cat = "a",
    .val = 3,
},
    ItemsItem{
    .cat = "a",
    .val = 1,
},
    ItemsItem{
    .cat = "b",
    .val = 5,
},
    ItemsItem{
    .cat = "b",
    .val = 2,
},
}; // []const ItemsItem
const ResultStruct0 = struct {
    cat: i32,
    total: i32,
};
const grouped = blk3: { var _tmp7 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator); var _tmp8 = std.StringHashMap(usize).init(std.heap.page_allocator); for (items) |i| { const _tmp9 = i.cat; if (_tmp8.get(_tmp9)) |idx| { _tmp7.items[idx].Items.append(i) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(ItemsItem) }{ .key = _tmp9, .Items = std.ArrayList(ItemsItem).init(std.heap.page_allocator) }; g.Items.append(i) catch |err| handleError(err); _tmp7.append(g) catch |err| handleError(err); _tmp8.put(_tmp9, _tmp7.items.len - 1) catch |err| handleError(err); } } var _tmp10 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp10.append(g) catch |err| handleError(err); } var _tmp11 = std.ArrayList(struct { item: struct { key: []const u8, Items: std.ArrayList(ItemsItem) }, key: i32 }).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(.{ .item = g, .key = -_sum_int(blk2: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp5.append(x.val) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; }) }) catch |err| handleError(err); } for (0.._tmp11.items.len) |i| { for (i+1.._tmp11.items.len) |j| { if (_tmp11.items[j].key < _tmp11.items[i].key) { const t = _tmp11.items[i]; _tmp11.items[i] = _tmp11.items[j]; _tmp11.items[j] = t; } } } var _tmp12 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator);for (_tmp11.items) |p| { _tmp12.append(p.item) catch |err| handleError(err); } var _tmp13 = std.ArrayList(struct {
    cat: i32,
    total: f64,
}).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(ResultStruct0{
    .cat = g.key,
    .total = _sum_int(blk1: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp3.append(x.val) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }),
}) catch |err| handleError(err); } const _tmp13Slice = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp13Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("{any}\n", .{grouped});
}
