const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch |err| handleError(err);
    std.debug.print("{s}\n", .{buf.items});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return std.meta.eql(a, b);
}

const NationItem = struct {
    n_nationkey: i32,
    n_name: []const u8,
};
const nation = &[_]NationItem{
    NationItem{
    .n_nationkey = 1,
    .n_name = "GERMANY",
},
    NationItem{
    .n_nationkey = 2,
    .n_name = "FRANCE",
},
}; // []const NationItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_nationkey: i32,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_nationkey = 1,
},
    SupplierItem{
    .s_suppkey = 200,
    .s_nationkey = 1,
},
    SupplierItem{
    .s_suppkey = 300,
    .s_nationkey = 2,
},
}; // []const SupplierItem
const PartsuppItem = struct {
    ps_partkey: i32,
    ps_suppkey: i32,
    ps_supplycost: f64,
    ps_availqty: i32,
};
const partsupp = &[_]PartsuppItem{
    PartsuppItem{
    .ps_partkey = 1000,
    .ps_suppkey = 100,
    .ps_supplycost = 10.0,
    .ps_availqty = 100,
},
    PartsuppItem{
    .ps_partkey = 1000,
    .ps_suppkey = 200,
    .ps_supplycost = 20.0,
    .ps_availqty = 50,
},
    PartsuppItem{
    .ps_partkey = 2000,
    .ps_suppkey = 100,
    .ps_supplycost = 5.0,
    .ps_availqty = 10,
},
    PartsuppItem{
    .ps_partkey = 3000,
    .ps_suppkey = 300,
    .ps_supplycost = 8.0,
    .ps_availqty = 500,
},
}; // []const PartsuppItem
const target_nation = "GERMANY"; // []const u8
const FilteredItem = struct {
    ps_partkey: i32,
    value: f64,
};
var filtered: []const FilteredItem = undefined; // []const FilteredItem
const GroupedItem = struct {
    ps_partkey: i32,
    value: f64,
};
const ResultStruct8 = struct { key: i32, Items: std.ArrayList(FilteredItem) };
var grouped: []const GroupedItem = undefined; // []const GroupedItem
const total = _sum_int(blk4: { var _tmp13 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { _tmp13.append(x.value) catch |err| handleError(err); } const _tmp14 = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp14; }); // f64
const threshold = (total * 0.0001); // f64
var result: []const GroupedItem = undefined; // []const GroupedItem

fn test_Q11_returns_high_value_partkeys_from_GERMANY() void {
    expect((result == &[_]FilteredItem{
    FilteredItem{
    .ps_partkey = 1000,
    .value = 2000.0,
},
    FilteredItem{
    .ps_partkey = 2000,
    .value = 50.0,
},
}));
}

pub fn main() void {
    filtered = blk0: { var _tmp1 = std.ArrayList(FilteredItem).init(std.heap.page_allocator); for (partsupp) |ps| { for (supplier) |s| { if (!((s.s_suppkey == ps.ps_suppkey))) continue; for (nation) |n| { if (!((n.n_nationkey == s.s_nationkey))) continue; if (!(std.mem.eql(u8, n.n_name, target_nation))) continue; _tmp1.append(FilteredItem{
    .ps_partkey = ps.ps_partkey,
    .value = (ps.ps_supplycost * ps.ps_availqty),
}) catch |err| handleError(err); } } } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp2; };
    grouped = blk3: { var _tmp9 = std.ArrayList(ResultStruct8).init(std.heap.page_allocator); for (filtered) |x| { const _tmp10 = x.ps_partkey; var _found = false; var _idx: usize = 0; for (_tmp9.items, 0..) |it, i| { if (_equal(it.key, _tmp10)) { _found = true; _idx = i; break; } } if (_found) { _tmp9.items[_idx].Items.append(x) catch |err| handleError(err); } else { var g = ResultStruct8{ .key = _tmp10, .Items = std.ArrayList(FilteredItem).init(std.heap.page_allocator) }; g.Items.append(x) catch |err| handleError(err); _tmp9.append(g) catch |err| handleError(err); } } var _tmp11 = std.ArrayList(ResultStruct8).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp11.append(g) catch |err| handleError(err); } var _tmp12 = std.ArrayList(GroupedItem).init(std.heap.page_allocator);for (_tmp11.items) |g| { _tmp12.append(FilteredItem{
    .ps_partkey = g.key,
    .value = _sum_int(blk2: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |r| { _tmp6.append(r.value) catch |err| handleError(err); } const _tmp7 = _tmp6.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp7; }),
}) catch |err| handleError(err); } const _tmp12Slice = _tmp12.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp12Slice; };
    result = blk5: { var _tmp15 = std.ArrayList(struct { item: GroupedItem, key: i32 }).init(std.heap.page_allocator); for (grouped) |x| { if (!((x.value > threshold))) continue; _tmp15.append(.{ .item = x, .key = -x.value }) catch |err| handleError(err); } for (0.._tmp15.items.len) |i| { for (i+1.._tmp15.items.len) |j| { if (_tmp15.items[j].key < _tmp15.items[i].key) { const t = _tmp15.items[i]; _tmp15.items[i] = _tmp15.items[j]; _tmp15.items[j] = t; } } } var _tmp16 = std.ArrayList(GroupedItem).init(std.heap.page_allocator);for (_tmp15.items) |p| { _tmp16.append(p.item) catch |err| handleError(err); } const _tmp17 = _tmp16.toOwnedSlice() catch |err| handleError(err); break :blk5 _tmp17; };
    _json(result);
    test_Q11_returns_high_value_partkeys_from_GERMANY();
}
