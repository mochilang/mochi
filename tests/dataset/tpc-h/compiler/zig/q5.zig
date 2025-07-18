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

const RegionItem = struct {
    r_regionkey: i32,
    r_name: []const u8,
};
const region = &[_]RegionItem{
    RegionItem{
    .r_regionkey = 0,
    .r_name = "ASIA",
},
    RegionItem{
    .r_regionkey = 1,
    .r_name = "EUROPE",
},
}; // []const RegionItem
const NationItem = struct {
    n_nationkey: i32,
    n_regionkey: i32,
    n_name: []const u8,
};
const nation = &[_]NationItem{
    NationItem{
    .n_nationkey = 10,
    .n_regionkey = 0,
    .n_name = "JAPAN",
},
    NationItem{
    .n_nationkey = 20,
    .n_regionkey = 0,
    .n_name = "INDIA",
},
    NationItem{
    .n_nationkey = 30,
    .n_regionkey = 1,
    .n_name = "FRANCE",
},
}; // []const NationItem
const CustomerItem = struct {
    c_custkey: i32,
    c_nationkey: i32,
};
const customer = &[_]CustomerItem{
    CustomerItem{
    .c_custkey = 1,
    .c_nationkey = 10,
},
    CustomerItem{
    .c_custkey = 2,
    .c_nationkey = 20,
},
}; // []const CustomerItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_nationkey: i32,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_nationkey = 10,
},
    SupplierItem{
    .s_suppkey = 200,
    .s_nationkey = 20,
},
}; // []const SupplierItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 1000,
    .o_custkey = 1,
    .o_orderdate = "1994-03-15",
},
    OrdersItem{
    .o_orderkey = 2000,
    .o_custkey = 2,
    .o_orderdate = "1994-06-10",
},
    OrdersItem{
    .o_orderkey = 3000,
    .o_custkey = 2,
    .o_orderdate = "1995-01-01",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_suppkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1000,
    .l_suppkey = 100,
    .l_extendedprice = 1000.0,
    .l_discount = 0.05,
},
    LineitemItem{
    .l_orderkey = 2000,
    .l_suppkey = 200,
    .l_extendedprice = 800.0,
    .l_discount = 0.1,
},
    LineitemItem{
    .l_orderkey = 3000,
    .l_suppkey = 200,
    .l_extendedprice = 900.0,
    .l_discount = 0.05,
},
}; // []const LineitemItem
var asia_nations: []const NationItem = undefined; // []const NationItem
const LocalCustomerSupplierOrdersItem = struct {
    nation: []const u8,
    revenue: f64,
};
var local_customer_supplier_orders: []const LocalCustomerSupplierOrdersItem = undefined; // []const LocalCustomerSupplierOrdersItem
const ResultItem = struct {
    n_name: i32,
    revenue: f64,
};
const ResultStruct12 = struct { key: i32, Items: std.ArrayList(LocalCustomerSupplierOrdersItem) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q5_returns_revenue_per_nation_in_ASIA_with_local_suppliers() void {
    expect((result == (blk6: { const _tmp19 = struct {
    n_name: []const u8,
    revenue: i32,
}; const _arr = &[_]_tmp19{
    _tmp19{
    .n_name = "JAPAN",
    .revenue = 950,
},
    _tmp19{
    .n_name = "INDIA",
    .revenue = 720,
},
}; break :blk6 _arr; })));
}

pub fn main() void {
    asia_nations = blk0: { var _tmp0 = std.ArrayList(NationItem).init(std.heap.page_allocator); for (region) |r| { for (nation) |n| { if (!((n.n_regionkey == r.r_regionkey))) continue; if (!(std.mem.eql(u8, r.r_name, "ASIA"))) continue; _tmp0.append(n) catch |err| handleError(err); } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    local_customer_supplier_orders = blk1: { var _tmp3 = std.ArrayList(LocalCustomerSupplierOrdersItem).init(std.heap.page_allocator); for (customer) |c| { for (asia_nations) |n| { if (!((c.c_nationkey == n.n_nationkey))) continue; for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; for (lineitem) |l| { if (!((l.l_orderkey == o.o_orderkey))) continue; for (supplier) |s| { if (!((s.s_suppkey == l.l_suppkey))) continue; if (!(((std.mem.order(u8, o.o_orderdate, "1994-01-01") != .lt and std.mem.order(u8, o.o_orderdate, "1995-01-01") == .lt) and (s.s_nationkey == c.c_nationkey)))) continue; _tmp3.append(LocalCustomerSupplierOrdersItem{
    .nation = n.n_name,
    .revenue = (l.l_extendedprice * ((1 - l.l_discount))),
}) catch |err| handleError(err); } } } } } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; };
    result = blk5: { var _tmp13 = std.ArrayList(ResultStruct12).init(std.heap.page_allocator); for (local_customer_supplier_orders) |r| { const _tmp14 = r.nation; var _found = false; var _idx: usize = 0; for (_tmp13.items, 0..) |it, i| { if (_equal(it.key, _tmp14)) { _found = true; _idx = i; break; } } if (_found) { _tmp13.items[_idx].Items.append(r) catch |err| handleError(err); } else { var g = ResultStruct12{ .key = _tmp14, .Items = std.ArrayList(LocalCustomerSupplierOrdersItem).init(std.heap.page_allocator) }; g.Items.append(r) catch |err| handleError(err); _tmp13.append(g) catch |err| handleError(err); } } var _tmp15 = std.ArrayList(ResultStruct12).init(std.heap.page_allocator);for (_tmp13.items) |g| { _tmp15.append(g) catch |err| handleError(err); } var _tmp16 = std.ArrayList(struct { item: ResultStruct12, key: i32 }).init(std.heap.page_allocator);for (_tmp15.items) |g| { _tmp16.append(.{ .item = g, .key = -_sum_int(blk4: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp10.append(x.revenue) catch |err| handleError(err); } const _tmp11 = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp11; }) }) catch |err| handleError(err); } for (0.._tmp16.items.len) |i| { for (i+1.._tmp16.items.len) |j| { if (_tmp16.items[j].key < _tmp16.items[i].key) { const t = _tmp16.items[i]; _tmp16.items[i] = _tmp16.items[j]; _tmp16.items[j] = t; } } } var _tmp17 = std.ArrayList(ResultStruct12).init(std.heap.page_allocator);for (_tmp16.items) |p| { _tmp17.append(p.item) catch |err| handleError(err); } var _tmp18 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp17.items) |g| { _tmp18.append(ResultItem{
    .n_name = g.key,
    .revenue = _sum_int(blk3: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp8.append(x.revenue) catch |err| handleError(err); } const _tmp9 = _tmp8.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp9; }),
}) catch |err| handleError(err); } const _tmp18Slice = _tmp18.toOwnedSlice() catch |err| handleError(err); break :blk5 _tmp18Slice; };
    _json(result);
    test_Q5_returns_revenue_per_nation_in_ASIA_with_local_suppliers();
}
