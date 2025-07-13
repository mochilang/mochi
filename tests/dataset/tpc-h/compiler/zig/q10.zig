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
const nation = &[_]NationItem{NationItem{
    .n_nationkey = 1,
    .n_name = "BRAZIL",
}}; // []const NationItem
const CustomerItem = struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_nationkey: i32,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
};
const customer = &[_]CustomerItem{CustomerItem{
    .c_custkey = 1,
    .c_name = "Alice",
    .c_acctbal = 100.0,
    .c_nationkey = 1,
    .c_address = "123 St",
    .c_phone = "123-456",
    .c_comment = "Loyal",
}}; // []const CustomerItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 1000,
    .o_custkey = 1,
    .o_orderdate = "1993-10-15",
},
    OrdersItem{
    .o_orderkey = 2000,
    .o_custkey = 1,
    .o_orderdate = "1994-01-02",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_returnflag: []const u8,
    l_extendedprice: f64,
    l_discount: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1000,
    .l_returnflag = "R",
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
},
    LineitemItem{
    .l_orderkey = 2000,
    .l_returnflag = "N",
    .l_extendedprice = 500.0,
    .l_discount = 0.0,
},
}; // []const LineitemItem
const start_date = "1993-10-01"; // []const u8
const end_date = "1994-01-01"; // []const u8
const ResultStruct0 = struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
};
const ResultItem = struct {
    c_custkey: i32,
    c_name: i32,
    revenue: i32,
    c_acctbal: i32,
    n_name: i32,
    c_address: i32,
    c_phone: i32,
    c_comment: i32,
};
const ResultStruct8 = struct {
    c: CustomerItem,
    o: OrdersItem,
    l: LineitemItem,
    n: NationItem,
};
const ResultStruct9 = struct { key: ResultStruct0, Items: std.ArrayList(ResultStruct8) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q10_returns_customer_revenue_from_returned_items() void {
    expect((result == &[_]ResultItem{ResultItem{
    .c_custkey = 1,
    .c_name = "Alice",
    .revenue = (1000.0 * 0.9),
    .c_acctbal = 100.0,
    .n_name = "BRAZIL",
    .c_address = "123 St",
    .c_phone = "123-456",
    .c_comment = "Loyal",
}}));
}

pub fn main() void {
    result = blk3: { var _tmp10 = std.ArrayList(ResultStruct9).init(std.heap.page_allocator); for (customer) |c| { for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; for (lineitem) |l| { if (!((l.l_orderkey == o.o_orderkey))) continue; for (nation) |n| { if (!((n.n_nationkey == c.c_nationkey))) continue; if (!(((std.mem.order(u8, o.o_orderdate, start_date) != .lt and std.mem.order(u8, o.o_orderdate, end_date) == .lt) and std.mem.eql(u8, l.l_returnflag, "R")))) continue; const _tmp11 = ResultStruct0{
    .c_custkey = c.c_custkey,
    .c_name = c.c_name,
    .c_acctbal = c.c_acctbal,
    .c_address = c.c_address,
    .c_phone = c.c_phone,
    .c_comment = c.c_comment,
    .n_name = n.n_name,
}; var _found = false; var _idx: usize = 0; for (_tmp10.items, 0..) |it, i| { if (_equal(it.key, _tmp11)) { _found = true; _idx = i; break; } } if (_found) { _tmp10.items[_idx].Items.append(ResultStruct8{ .c = c, .o = o, .l = l, .n = n }) catch |err| handleError(err); } else { var g = ResultStruct9{ .key = _tmp11, .Items = std.ArrayList(ResultStruct8).init(std.heap.page_allocator) }; g.Items.append(ResultStruct8{ .c = c, .o = o, .l = l, .n = n }) catch |err| handleError(err); _tmp10.append(g) catch |err| handleError(err); } } } } } var _tmp12 = std.ArrayList(ResultStruct9).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp12.append(g) catch |err| handleError(err); } var _tmp13 = std.ArrayList(struct { item: ResultStruct9, key: i32 }).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(.{ .item = g, .key = -_sum_int(blk2: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp6.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp7 = _tmp6.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp7; }) }) catch |err| handleError(err); } for (0.._tmp13.items.len) |i| { for (i+1.._tmp13.items.len) |j| { if (_tmp13.items[j].key < _tmp13.items[i].key) { const t = _tmp13.items[i]; _tmp13.items[i] = _tmp13.items[j]; _tmp13.items[j] = t; } } } var _tmp14 = std.ArrayList(ResultStruct9).init(std.heap.page_allocator);for (_tmp13.items) |p| { _tmp14.append(p.item) catch |err| handleError(err); } var _tmp15 = std.ArrayList(struct {
    c_custkey: i32,
    c_name: []const u8,
    revenue: i32,
    c_acctbal: f64,
    n_name: []const u8,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
}).init(std.heap.page_allocator);for (_tmp14.items) |g| { _tmp15.append(ResultItem{
    .c_custkey = g.key.c_custkey,
    .c_name = g.key.c_name,
    .revenue = _sum_int(blk1: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp4.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp5; }),
    .c_acctbal = g.key.c_acctbal,
    .n_name = g.key.n_name,
    .c_address = g.key.c_address,
    .c_phone = g.key.c_phone,
    .c_comment = g.key.c_comment,
}) catch |err| handleError(err); } const _tmp15Slice = _tmp15.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp15Slice; };
    _json(result);
    test_Q10_returns_customer_revenue_from_returned_items();
}
