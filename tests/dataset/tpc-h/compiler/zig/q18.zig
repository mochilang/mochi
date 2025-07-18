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
    .n_name = "GERMANY",
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
const customer = &[_]CustomerItem{
    CustomerItem{
    .c_custkey = 1,
    .c_name = "Alice",
    .c_acctbal = 1000.0,
    .c_nationkey = 1,
    .c_address = "123 Market St",
    .c_phone = "123-456",
    .c_comment = "Premium client",
},
    CustomerItem{
    .c_custkey = 2,
    .c_name = "Bob",
    .c_acctbal = 200.0,
    .c_nationkey = 1,
    .c_address = "456 Side St",
    .c_phone = "987-654",
    .c_comment = "Frequent returns",
},
}; // []const CustomerItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 100,
    .o_custkey = 1,
},
    OrdersItem{
    .o_orderkey = 200,
    .o_custkey = 1,
},
    OrdersItem{
    .o_orderkey = 300,
    .o_custkey = 2,
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_quantity: i32,
    l_extendedprice: f64,
    l_discount: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 100,
    .l_quantity = 150,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
},
    LineitemItem{
    .l_orderkey = 200,
    .l_quantity = 100,
    .l_extendedprice = 800.0,
    .l_discount = 0.0,
},
    LineitemItem{
    .l_orderkey = 300,
    .l_quantity = 30,
    .l_extendedprice = 300.0,
    .l_discount = 0.05,
},
}; // []const LineitemItem
const threshold = 200; // i32
const ResultStruct0 = struct {
    c_name: []const u8,
    c_custkey: i32,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
};
const ResultItem = struct {
    c_name: []const u8,
    c_custkey: i32,
    revenue: i32,
    c_acctbal: f64,
    n_name: []const u8,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
};
const ResultStruct10 = struct {
    c: CustomerItem,
    o: OrdersItem,
    l: LineitemItem,
    n: NationItem,
};
const ResultStruct11 = struct { key: ResultStruct0, Items: std.ArrayList(ResultStruct10) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q18_returns_large_volume_customers_with_total_quantity___200() void {
    expect((result == &[_]ResultItem{ResultItem{
    .c_name = "Alice",
    .c_custkey = 1,
    .revenue = 1700.0,
    .c_acctbal = 1000.0,
    .n_name = "GERMANY",
    .c_address = "123 Market St",
    .c_phone = "123-456",
    .c_comment = "Premium client",
}}));
}

pub fn main() void {
    result = blk4: { var _tmp12 = std.ArrayList(ResultStruct11).init(std.heap.page_allocator); for (customer) |c| { for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; for (lineitem) |l| { if (!((l.l_orderkey == o.o_orderkey))) continue; for (nation) |n| { if (!((n.n_nationkey == c.c_nationkey))) continue; const _tmp13 = ResultStruct0{
    .c_name = c.c_name,
    .c_custkey = c.c_custkey,
    .c_acctbal = c.c_acctbal,
    .c_address = c.c_address,
    .c_phone = c.c_phone,
    .c_comment = c.c_comment,
    .n_name = n.n_name,
}; var _found = false; var _idx: usize = 0; for (_tmp12.items, 0..) |it, i| { if (_equal(it.key, _tmp13)) { _found = true; _idx = i; break; } } if (_found) { _tmp12.items[_idx].Items.append(ResultStruct10{ .c = c, .o = o, .l = l, .n = n }) catch |err| handleError(err); } else { var g = ResultStruct11{ .key = _tmp13, .Items = std.ArrayList(ResultStruct10).init(std.heap.page_allocator) }; g.Items.append(ResultStruct10{ .c = c, .o = o, .l = l, .n = n }) catch |err| handleError(err); _tmp12.append(g) catch |err| handleError(err); } } } } } var _tmp14 = std.ArrayList(ResultStruct11).init(std.heap.page_allocator);for (_tmp12.items) |g| { if (!((_sum_int(blk3: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp8.append(x.l.l_quantity) catch |err| handleError(err); } const _tmp9 = _tmp8.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp9; }) > threshold))) continue; _tmp14.append(g) catch |err| handleError(err); } var _tmp15 = std.ArrayList(struct { item: ResultStruct11, key: i32 }).init(std.heap.page_allocator);for (_tmp14.items) |g| { _tmp15.append(.{ .item = g, .key = -_sum_int(blk2: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp6.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp7 = _tmp6.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp7; }) }) catch |err| handleError(err); } for (0.._tmp15.items.len) |i| { for (i+1.._tmp15.items.len) |j| { if (_tmp15.items[j].key < _tmp15.items[i].key) { const t = _tmp15.items[i]; _tmp15.items[i] = _tmp15.items[j]; _tmp15.items[j] = t; } } } var _tmp16 = std.ArrayList(ResultStruct11).init(std.heap.page_allocator);for (_tmp15.items) |p| { _tmp16.append(p.item) catch |err| handleError(err); } var _tmp17 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp16.items) |g| { _tmp17.append(ResultItem{
    .c_name = g.key.c_name,
    .c_custkey = g.key.c_custkey,
    .revenue = _sum_int(blk1: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp4.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp5; }),
    .c_acctbal = g.key.c_acctbal,
    .n_name = g.key.n_name,
    .c_address = g.key.c_address,
    .c_phone = g.key.c_phone,
    .c_comment = g.key.c_comment,
}) catch |err| handleError(err); } const _tmp17Slice = _tmp17.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp17Slice; };
    _json(result);
    test_Q18_returns_large_volume_customers_with_total_quantity___200();
}
