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
const region = &[_]RegionItem{RegionItem{
    .r_regionkey = 0,
    .r_name = "AMERICA",
}}; // []const RegionItem
const NationItem = struct {
    n_nationkey: i32,
    n_regionkey: i32,
    n_name: []const u8,
};
const nation = &[_]NationItem{
    NationItem{
    .n_nationkey = 10,
    .n_regionkey = 0,
    .n_name = "BRAZIL",
},
    NationItem{
    .n_nationkey = 20,
    .n_regionkey = 0,
    .n_name = "CANADA",
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
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 100,
    .o_custkey = 1,
    .o_orderdate = "1995-04-10",
},
    OrdersItem{
    .o_orderkey = 200,
    .o_custkey = 2,
    .o_orderdate = "1995-07-15",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_suppkey: i32,
    l_partkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 100,
    .l_suppkey = 1000,
    .l_partkey = 5000,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
},
    LineitemItem{
    .l_orderkey = 200,
    .l_suppkey = 2000,
    .l_partkey = 5000,
    .l_extendedprice = 500.0,
    .l_discount = 0.05,
},
}; // []const LineitemItem
const SupplierItem = struct { s_suppkey: i32, };
const supplier = &[_]SupplierItem{
    SupplierItem{ .s_suppkey = 1000 },
    SupplierItem{ .s_suppkey = 2000 },
}; // []const SupplierItem
const PartItem = struct {
    p_partkey: i32,
    p_type: []const u8,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 5000,
    .p_type = "ECONOMY ANODIZED STEEL",
},
    PartItem{
    .p_partkey = 6000,
    .p_type = "SMALL BRASS",
},
}; // []const PartItem
const start_date = "1995-01-01"; // []const u8
const end_date = "1996-12-31"; // []const u8
const target_type = "ECONOMY ANODIZED STEEL"; // []const u8
const target_nation = "BRAZIL"; // []const u8
const ResultItem = struct {
    o_year: []const u8,
    mkt_share: i32,
};
const ResultStruct9 = struct {
    l: LineitemItem,
    p: PartItem,
    s: SupplierItem,
    o: OrdersItem,
    c: CustomerItem,
    n: NationItem,
    r: RegionItem,
};
const ResultStruct10 = struct { key: []const u8, Items: std.ArrayList(ResultStruct9) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q8_returns_correct_market_share_for_BRAZIL_in_1995() void {
    const numerator = (1000.0 * 0.9); // f64
    const denominator = (numerator + ((500.0 * 0.95))); // f64
    const share = (numerator / denominator); // f64
    expect((result == &[_]ResultItem{ResultItem{
    .o_year = "1995",
    .mkt_share = share,
}}));
}

pub fn main() void {
    result = blk4: { var _tmp11 = std.ArrayList(ResultStruct10).init(std.heap.page_allocator); for (lineitem) |l| { for (part) |p| { if (!((p.p_partkey == l.l_partkey))) continue; for (supplier) |s| { if (!((s.s_suppkey == l.l_suppkey))) continue; for (orders) |o| { if (!((o.o_orderkey == l.l_orderkey))) continue; for (customer) |c| { if (!((c.c_custkey == o.o_custkey))) continue; for (nation) |n| { if (!((n.n_nationkey == c.c_nationkey))) continue; for (region) |r| { if (!((r.r_regionkey == n.n_regionkey))) continue; if (!(((((std.mem.eql(u8, p.p_type, target_type) and std.mem.order(u8, o.o_orderdate, start_date) != .lt) and std.mem.order(u8, o.o_orderdate, end_date) != .gt) and std.mem.eql(u8, r.r_name, "AMERICA"))))) continue; const _tmp12 = substring(o.o_orderdate, 0, 4); var _found = false; var _idx: usize = 0; for (_tmp11.items, 0..) |it, i| { if (_equal(it.key, _tmp12)) { _found = true; _idx = i; break; } } if (_found) { _tmp11.items[_idx].Items.append(ResultStruct9{ .l = l, .p = p, .s = s, .o = o, .c = c, .n = n, .r = r }) catch |err| handleError(err); } else { var g = ResultStruct10{ .key = _tmp12, .Items = std.ArrayList(ResultStruct9).init(std.heap.page_allocator) }; g.Items.append(ResultStruct9{ .l = l, .p = p, .s = s, .o = o, .c = c, .n = n, .r = r }) catch |err| handleError(err); _tmp11.append(g) catch |err| handleError(err); } } } } } } } } var _tmp13 = std.ArrayList(ResultStruct10).init(std.heap.page_allocator);for (_tmp11.items) |year| { _tmp13.append(year) catch |err| handleError(err); } var _tmp14 = std.ArrayList(struct { item: ResultStruct10, key: i32 }).init(std.heap.page_allocator);for (_tmp13.items) |year| { _tmp14.append(.{ .item = year, .key = year.key }) catch |err| handleError(err); } for (0.._tmp14.items.len) |i| { for (i+1.._tmp14.items.len) |j| { if (_tmp14.items[j].key < _tmp14.items[i].key) { const t = _tmp14.items[i]; _tmp14.items[i] = _tmp14.items[j]; _tmp14.items[j] = t; } } } var _tmp15 = std.ArrayList(ResultStruct10).init(std.heap.page_allocator);for (_tmp14.items) |p| { _tmp15.append(p.item) catch |err| handleError(err); } var _tmp16 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp15.items) |year| { _tmp16.append(ResultItem{
    .o_year = year.key,
    .mkt_share = (_sum_int(blk2: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (year.Items.items) |x| { _tmp5.append(switch (std.mem.eql(u8, x.n.n_name, target_nation)) {true => (x.l.l_extendedprice * ((1 - x.l.l_discount))), else => 0.0, }) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; }) / _sum_int(blk3: { var _tmp7 = std.ArrayList(i32).init(std.heap.page_allocator); for (year.Items.items) |x| { _tmp7.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp8 = _tmp7.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp8; })),
}) catch |err| handleError(err); } const _tmp16Slice = _tmp16.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp16Slice; };
    _json(result);
    test_Q8_returns_correct_market_share_for_BRAZIL_in_1995();
}
