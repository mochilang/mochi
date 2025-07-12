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
const ResultStruct0 = struct {
    cat: i32,
    share: i32,
};
const result = blk4: { var _tmp9 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator); var _tmp10 = std.StringHashMap(usize).init(std.heap.page_allocator); for (items) |i| { const _tmp11 = i.cat; if (_tmp10.get(_tmp11)) |idx| { _tmp9.items[idx].Items.append(i) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(ItemsItem) }{ .key = _tmp11, .Items = std.ArrayList(ItemsItem).init(std.heap.page_allocator) }; g.Items.append(i) catch |err| handleError(err); _tmp9.append(g) catch |err| handleError(err); _tmp10.put(_tmp11, _tmp9.items.len - 1) catch |err| handleError(err); } } var _tmp12 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp12.append(g) catch |err| handleError(err); } var _tmp13 = std.ArrayList(struct { item: struct { key: []const u8, Items: std.ArrayList(ItemsItem) }, key: i32 }).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(.{ .item = g, .key = g.key }) catch |err| handleError(err); } for (0.._tmp13.items.len) |i| { for (i+1.._tmp13.items.len) |j| { if (_tmp13.items[j].key < _tmp13.items[i].key) { const t = _tmp13.items[i]; _tmp13.items[i] = _tmp13.items[j]; _tmp13.items[j] = t; } } } var _tmp14 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(ItemsItem) }).init(std.heap.page_allocator);for (_tmp13.items) |p| { _tmp14.append(p.item) catch |err| handleError(err); } var _tmp15 = std.ArrayList(struct {
    cat: i32,
    share: f64,
}).init(std.heap.page_allocator);for (_tmp14.items) |g| { _tmp15.append(ResultStruct0{
    .cat = g.key,
    .share = (_sum_int(blk2: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp5.append(if (x.flag) (x.val) else (0)) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; }) / _sum_int(blk3: { var _tmp7 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp7.append(x.val) catch |err| handleError(err); } const _tmp8 = _tmp7.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp8; })),
}) catch |err| handleError(err); } const _tmp15Slice = _tmp15.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp15Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("{any}\n", .{result});
}
