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
    .n_name = "FRANCE",
},
    NationItem{
    .n_nationkey = 2,
    .n_name = "GERMANY",
},
}; // []const NationItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_nationkey: i32,
};
const supplier = &[_]SupplierItem{SupplierItem{
    .s_suppkey = 100,
    .s_nationkey = 1,
}}; // []const SupplierItem
const CustomerItem = struct {
    c_custkey: i32,
    c_nationkey: i32,
};
const customer = &[_]CustomerItem{CustomerItem{
    .c_custkey = 200,
    .c_nationkey = 2,
}}; // []const CustomerItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
};
const orders = &[_]OrdersItem{OrdersItem{
    .o_orderkey = 1000,
    .o_custkey = 200,
}}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_suppkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1000,
    .l_suppkey = 100,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
    .l_shipdate = "1995-06-15",
},
    LineitemItem{
    .l_orderkey = 1000,
    .l_suppkey = 100,
    .l_extendedprice = 800.0,
    .l_discount = 0.05,
    .l_shipdate = "1997-01-01",
},
}; // []const LineitemItem
const start_date = "1995-01-01"; // []const u8
const end_date = "1996-12-31"; // []const u8
const nation1 = "FRANCE"; // []const u8
const nation2 = "GERMANY"; // []const u8
const ResultStruct0 = struct {
    supp_nation: []const u8,
    cust_nation: []const u8,
    l_year: []const u8,
};
const ResultItem = struct {
    supp_nation: i32,
    cust_nation: i32,
    l_year: i32,
    revenue: i32,
};
const ResultStruct6 = struct {
    l: LineitemItem,
    o: OrdersItem,
    c: CustomerItem,
    s: SupplierItem,
    n1: NationItem,
    n2: NationItem,
};
const ResultStruct7 = struct { key: ResultStruct0, Items: std.ArrayList(ResultStruct6) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q7_computes_revenue_between_FRANCE_and_GERMANY_by_year() void {
    expect((result == &[_]ResultItem{ResultItem{
    .supp_nation = "FRANCE",
    .cust_nation = "GERMANY",
    .l_year = "1995",
    .revenue = 900.0,
}}));
}

pub fn main() void {
    result = blk2: { var _tmp8 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator); for (lineitem) |l| { for (orders) |o| { if (!((o.o_orderkey == l.l_orderkey))) continue; for (customer) |c| { if (!((c.c_custkey == o.o_custkey))) continue; for (supplier) |s| { if (!((s.s_suppkey == l.l_suppkey))) continue; for (nation) |n1| { if (!((n1.n_nationkey == s.s_nationkey))) continue; for (nation) |n2| { if (!((n2.n_nationkey == c.c_nationkey))) continue; if (!(((((std.mem.order(u8, l.l_shipdate, start_date) != .lt and std.mem.order(u8, l.l_shipdate, end_date) != .gt) and ((std.mem.eql(u8, n1.n_name, nation1) and std.mem.eql(u8, n2.n_name, nation2)))) or ((std.mem.eql(u8, n1.n_name, nation2) and std.mem.eql(u8, n2.n_name, nation1))))))) continue; const _tmp9 = ResultStruct0{
    .supp_nation = n1.n_name,
    .cust_nation = n2.n_name,
    .l_year = substring(l.l_shipdate, 0, 4),
}; var _found = false; var _idx: usize = 0; for (_tmp8.items, 0..) |it, i| { if (_equal(it.key, _tmp9)) { _found = true; _idx = i; break; } } if (_found) { _tmp8.items[_idx].Items.append(ResultStruct6{ .l = l, .o = o, .c = c, .s = s, .n1 = n1, .n2 = n2 }) catch |err| handleError(err); } else { var g = ResultStruct7{ .key = _tmp9, .Items = std.ArrayList(ResultStruct6).init(std.heap.page_allocator) }; g.Items.append(ResultStruct6{ .l = l, .o = o, .c = c, .s = s, .n1 = n1, .n2 = n2 }) catch |err| handleError(err); _tmp8.append(g) catch |err| handleError(err); } } } } } } } var _tmp10 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp10.append(g) catch |err| handleError(err); } var _tmp11 = std.ArrayList(struct { item: ResultStruct7, key: []const i32 }).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(.{ .item = g, .key = &[_]i32{
    "supp_nation",
    "cust_nation",
    "l_year",
} }) catch |err| handleError(err); } for (0.._tmp11.items.len) |i| { for (i+1.._tmp11.items.len) |j| { if (_tmp11.items[j].key < _tmp11.items[i].key) { const t = _tmp11.items[i]; _tmp11.items[i] = _tmp11.items[j]; _tmp11.items[j] = t; } } } var _tmp12 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp11.items) |p| { _tmp12.append(p.item) catch |err| handleError(err); } var _tmp13 = std.ArrayList(struct {
    supp_nation: []const u8,
    cust_nation: []const u8,
    l_year: []const u8,
    revenue: i32,
}).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(ResultItem{
    .supp_nation = g.key.supp_nation,
    .cust_nation = g.key.cust_nation,
    .l_year = g.key.l_year,
    .revenue = _sum_int(blk1: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp4.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp5; }),
}) catch |err| handleError(err); } const _tmp13Slice = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp13Slice; };
    _json(result);
    test_Q7_computes_revenue_between_FRANCE_and_GERMANY_by_year();
}
