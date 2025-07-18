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

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
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
    .n_name = "CANADA",
},
    NationItem{
    .n_nationkey = 2,
    .n_name = "GERMANY",
},
}; // []const NationItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_name: []const u8,
    s_address: []const u8,
    s_nationkey: i32,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_name = "Maple Supply",
    .s_address = "123 Forest Lane",
    .s_nationkey = 1,
},
    SupplierItem{
    .s_suppkey = 200,
    .s_name = "Berlin Metals",
    .s_address = "456 Iron Str",
    .s_nationkey = 2,
},
}; // []const SupplierItem
const PartItem = struct {
    p_partkey: i32,
    p_name: []const u8,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 10,
    .p_name = "forest glade bricks",
},
    PartItem{
    .p_partkey = 20,
    .p_name = "desert sand paper",
},
}; // []const PartItem
const PartsuppItem = struct {
    ps_partkey: i32,
    ps_suppkey: i32,
    ps_availqty: i32,
};
const partsupp = &[_]PartsuppItem{
    PartsuppItem{
    .ps_partkey = 10,
    .ps_suppkey = 100,
    .ps_availqty = 100,
},
    PartsuppItem{
    .ps_partkey = 20,
    .ps_suppkey = 200,
    .ps_availqty = 30,
},
}; // []const PartsuppItem
const LineitemItem = struct {
    l_partkey: i32,
    l_suppkey: i32,
    l_quantity: i32,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_partkey = 10,
    .l_suppkey = 100,
    .l_quantity = 100,
    .l_shipdate = "1994-05-15",
},
    LineitemItem{
    .l_partkey = 10,
    .l_suppkey = 100,
    .l_quantity = 50,
    .l_shipdate = "1995-01-01",
},
}; // []const LineitemItem
const prefix = "forest"; // []const u8
const ResultStruct0 = struct {
    partkey: i32,
    suppkey: i32,
};
const Shipped94Item = struct {
    partkey: i32,
    suppkey: i32,
    qty: f64,
};
const ResultStruct6 = struct { key: ResultStruct0, Items: std.ArrayList(LineitemItem) };
var shipped_94: []const Shipped94Item = undefined; // []const Shipped94Item
var target_partkeys: []const i32 = undefined; // []const i32
const ResultItem = struct {
    s_name: []const u8,
    s_address: []const u8,
};
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q20_returns_suppliers_from_CANADA_with_forest_part_stock___50__of_1994_shipments() void {
    expect((result == &[_]ResultItem{ResultItem{
    .s_name = "Maple Supply",
    .s_address = "123 Forest Lane",
}}));
}

pub fn main() void {
    shipped_94 = blk2: { var _tmp7 = std.ArrayList(ResultStruct6).init(std.heap.page_allocator); for (lineitem) |l| { if (!((std.mem.order(u8, l.l_shipdate, "1994-01-01") != .lt and std.mem.order(u8, l.l_shipdate, "1995-01-01") == .lt))) continue; const _tmp8 = ResultStruct0{
    .partkey = l.l_partkey,
    .suppkey = l.l_suppkey,
}; var _found = false; var _idx: usize = 0; for (_tmp7.items, 0..) |it, i| { if (_equal(it.key, _tmp8)) { _found = true; _idx = i; break; } } if (_found) { _tmp7.items[_idx].Items.append(l) catch |err| handleError(err); } else { var g = ResultStruct6{ .key = _tmp8, .Items = std.ArrayList(LineitemItem).init(std.heap.page_allocator) }; g.Items.append(l) catch |err| handleError(err); _tmp7.append(g) catch |err| handleError(err); } } var _tmp9 = std.ArrayList(ResultStruct6).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp9.append(g) catch |err| handleError(err); } var _tmp10 = std.ArrayList(Shipped94Item).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp10.append(Shipped94Item{
    .partkey = g.key.partkey,
    .suppkey = g.key.suppkey,
    .qty = _sum_int(blk1: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp4.append(x.l_quantity) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp5; }),
}) catch |err| handleError(err); } const _tmp10Slice = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp10Slice; };
    target_partkeys = blk3: { var _tmp11 = std.ArrayList(i32).init(std.heap.page_allocator); for (partsupp) |ps| { for (part) |p| { if (!((ps.ps_partkey == p.p_partkey))) continue; for (shipped_94) |s| { if (!(((ps.ps_partkey == s.partkey) and (ps.ps_suppkey == s.suppkey)))) continue; if (!((std.mem.eql(u8, substring(p.p_name, 0, @as(i32, @intCast((prefix).len))), prefix) and (ps.ps_availqty > ((0.5 * s.qty)))))) continue; _tmp11.append(ps.ps_suppkey) catch |err| handleError(err); } } } const _tmp12 = _tmp11.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp12; };
    result = blk4: { var _tmp14 = std.ArrayList(ResultItem).init(std.heap.page_allocator); for (supplier) |s| { for (nation) |n| { if (!((n.n_nationkey == s.s_nationkey))) continue; if (!((_contains_list_int(target_partkeys, s.s_suppkey) and std.mem.eql(u8, n.n_name, "CANADA")))) continue; _tmp14.append(ResultItem{
    .s_name = s.s_name,
    .s_address = s.s_address,
}) catch |err| handleError(err); } } const _tmp15 = _tmp14.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp15; };
    _json(result);
    test_Q20_returns_suppliers_from_CANADA_with_forest_part_stock___50__of_1994_shipments();
}
