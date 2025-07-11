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

const NationsItem = struct {
    id: i32,
    name: []const u8,
};
const nations = &[_]NationsItem{
    NationsItem{
    .id = 1,
    .name = "A",
},
    NationsItem{
    .id = 2,
    .name = "B",
},
};
const SuppliersItem = struct {
    id: i32,
    nation: i32,
};
const suppliers = &[_]SuppliersItem{
    SuppliersItem{
    .id = 1,
    .nation = 1,
},
    SuppliersItem{
    .id = 2,
    .nation = 2,
},
};
const PartsuppItem = struct {
    part: i32,
    supplier: i32,
    cost: f64,
    qty: i32,
};
const partsupp = &[_]PartsuppItem{
    PartsuppItem{
    .part = 100,
    .supplier = 1,
    .cost = 10.0,
    .qty = 2,
},
    PartsuppItem{
    .part = 100,
    .supplier = 2,
    .cost = 20.0,
    .qty = 1,
},
    PartsuppItem{
    .part = 200,
    .supplier = 1,
    .cost = 5.0,
    .qty = 3,
},
};
const filtered = blk0: { var _tmp0 = std.ArrayList(struct {
    part: i32,
    value: f64,
}).init(std.heap.page_allocator); for (partsupp) |ps| { for (suppliers) |s| { if (!((s.id == ps.supplier))) continue; for (nations) |n| { if (!((n.id == s.nation))) continue; if (!(std.mem.eql(u8, n.name, "A"))) continue; _tmp0.append(struct {
    part: i32,
    value: f64,
}{
    .part = ps.part,
    .value = (ps.cost * ps.qty),
}) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; };
const grouped = blk2: { var _tmp4 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.StringHashMap(i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (filtered) |x| { const _tmp6 = x.part; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(x) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.StringHashMap(i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.StringHashMap(i32)).init(std.heap.page_allocator) }; g.Items.append(x) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } var _tmp7 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.StringHashMap(i32)) }).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(g) catch unreachable; } var _tmp8 = std.ArrayList(struct {
    part: i32,
    total: f64,
}).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp8.append(struct {
    part: i32,
    total: f64,
}{
    .part = g.key,
    .total = _sum_int(blk1: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |r| { _tmp2.append(r.value) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk1 _tmp3; }),
}) catch unreachable; } const _tmp8Slice = _tmp8.toOwnedSlice() catch unreachable; break :blk2 _tmp8Slice; };

pub fn main() void {
    std.debug.print("{any}\n", .{grouped});
}
