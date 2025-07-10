const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _print_list(comptime T: type, v: []const T) void {
    for (v, 0..) |it, i| {
        if (i > 0) std.debug.print(" ", .{});
        std.debug.print("{any}", .{it});
    }
    std.debug.print("\n", .{});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const nations = (blk0: { const _tmp0 = struct {
    id: i32,
    name: []const u8,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .id = 1,
    .name = "A",
},
    _tmp0{
    .id = 2,
    .name = "B",
},
}; break :blk0 _arr; });
const suppliers = (blk1: { const _tmp1 = struct {
    id: i32,
    nation: i32,
}; const _arr = &[_]_tmp1{
    _tmp1{
    .id = 1,
    .nation = 1,
},
    _tmp1{
    .id = 2,
    .nation = 2,
},
}; break :blk1 _arr; });
const partsupp = (blk2: { const _tmp2 = struct {
    part: i32,
    supplier: i32,
    cost: f64,
    qty: i32,
}; const _arr = &[_]_tmp2{
    _tmp2{
    .part = 100,
    .supplier = 1,
    .cost = 10,
    .qty = 2,
},
    _tmp2{
    .part = 100,
    .supplier = 2,
    .cost = 20,
    .qty = 1,
},
    _tmp2{
    .part = 200,
    .supplier = 1,
    .cost = 5,
    .qty = 3,
},
}; break :blk2 _arr; });
const filtered = blk3: { var _tmp3 = std.ArrayList(struct {
    part: i32,
    value: f64,
}).init(std.heap.page_allocator); for (partsupp) |ps| { for (suppliers) |s| { if (!((s.id == ps.supplier))) continue; for (nations) |n| { if (!((n.id == s.nation))) continue; if (!(std.mem.eql(u8, n.name, "A"))) continue; _tmp3.append(struct {
    part: i32,
    value: f64,
}{
    .part = ps.part,
    .value = (ps.cost * ps.qty),
}) catch unreachable; } } } const _tmp4 = _tmp3.toOwnedSlice() catch unreachable; break :blk3 _tmp4; };
const grouped = blk5: { var _tmp7 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.StringHashMap(i32)) }).init(std.heap.page_allocator); var _tmp8 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (filtered) |x| { const _tmp9 = x.part; if (_tmp8.get(_tmp9)) |idx| { _tmp7.items[idx].Items.append(x) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.StringHashMap(i32)) }{ .key = _tmp9, .Items = std.ArrayList(std.StringHashMap(i32)).init(std.heap.page_allocator) }; g.Items.append(x) catch unreachable; _tmp7.append(g) catch unreachable; _tmp8.put(_tmp9, _tmp7.items.len - 1) catch unreachable; } } var _tmp10 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.StringHashMap(i32)) }).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp10.append(g) catch unreachable; } var _tmp11 = std.ArrayList(struct {
    part: i32,
    total: f64,
}).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(struct {
    part: i32,
    total: f64,
}{
    .part = g.key,
    .total = _sum_int(blk4: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |r| { _tmp5.append(r.value) catch unreachable; } const _tmp6 = _tmp5.toOwnedSlice() catch unreachable; break :blk4 _tmp6; }),
}) catch unreachable; } const _tmp11Slice = _tmp11.toOwnedSlice() catch unreachable; break :blk5 _tmp11Slice; };

pub fn main() void {
    _print_list(std.StringHashMap(i32), grouped);
}
