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

const CustomerItem = struct {
    c_custkey: i32,
    c_mktsegment: []const u8,
};
const customer = &[_]CustomerItem{
    CustomerItem{
    .c_custkey = 1,
    .c_mktsegment = "BUILDING",
},
    CustomerItem{
    .c_custkey = 2,
    .c_mktsegment = "AUTOMOBILE",
},
}; // []const CustomerItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: []const u8,
    o_shippriority: i32,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 100,
    .o_custkey = 1,
    .o_orderdate = "1995-03-14",
    .o_shippriority = 1,
},
    OrdersItem{
    .o_orderkey = 200,
    .o_custkey = 2,
    .o_orderdate = "1995-03-10",
    .o_shippriority = 2,
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 100,
    .l_extendedprice = 1000.0,
    .l_discount = 0.05,
    .l_shipdate = "1995-03-16",
},
    LineitemItem{
    .l_orderkey = 100,
    .l_extendedprice = 500.0,
    .l_discount = 0.0,
    .l_shipdate = "1995-03-20",
},
    LineitemItem{
    .l_orderkey = 200,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
    .l_shipdate = "1995-03-14",
},
}; // []const LineitemItem
const cutoff = "1995-03-15"; // []const u8
const segment = "BUILDING"; // []const u8
var building_customers: []const CustomerItem = undefined; // []const CustomerItem
var valid_orders: []const OrdersItem = undefined; // []const OrdersItem
var valid_lineitems: []const LineitemItem = undefined; // []const LineitemItem
const ResultStruct6 = struct {
    o_orderkey: i32,
    o_orderdate: []const u8,
    o_shippriority: i32,
};
const OrderLineJoinItem = struct {
    l_orderkey: i32,
    revenue: i32,
    o_orderdate: i32,
    o_shippriority: i32,
};
const ResultStruct14 = struct {
    o: OrdersItem,
    l: LineitemItem,
};
const ResultStruct15 = struct { key: ResultStruct6, Items: std.ArrayList(ResultStruct14) };
var order_line_join: []const OrderLineJoinItem = undefined; // []const OrderLineJoinItem

fn test_Q3_returns_revenue_per_order_with_correct_priority() void {
    expect((order_line_join == &[_]OrderLineJoinItem{OrderLineJoinItem{
    .l_orderkey = 100,
    .revenue = ((1000.0 * 0.95) + 500.0),
    .o_orderdate = "1995-03-14",
    .o_shippriority = 1,
}}));
}

pub fn main() void {
    building_customers = blk0: { var _tmp0 = std.ArrayList(CustomerItem).init(std.heap.page_allocator); for (customer) |c| { if (!(std.mem.eql(u8, c.c_mktsegment, segment))) continue; _tmp0.append(c) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    valid_orders = blk1: { var _tmp2 = std.ArrayList(OrdersItem).init(std.heap.page_allocator); for (orders) |o| { for (building_customers) |c| { if (!((o.o_custkey == c.c_custkey))) continue; if (!(std.mem.order(u8, o.o_orderdate, cutoff) == .lt)) continue; _tmp2.append(o) catch |err| handleError(err); } } const _tmp3 = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp3; };
    valid_lineitems = blk2: { var _tmp4 = std.ArrayList(LineitemItem).init(std.heap.page_allocator); for (lineitem) |l| { if (!(std.mem.order(u8, l.l_shipdate, cutoff) == .gt)) continue; _tmp4.append(l) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp5; };
    order_line_join = blk6: { var _tmp16 = std.ArrayList(ResultStruct15).init(std.heap.page_allocator); for (valid_orders) |o| { for (valid_lineitems) |l| { if (!((l.l_orderkey == o.o_orderkey))) continue; const _tmp17 = ResultStruct6{
    .o_orderkey = o.o_orderkey,
    .o_orderdate = o.o_orderdate,
    .o_shippriority = o.o_shippriority,
}; var _found = false; var _idx: usize = 0; for (_tmp16.items, 0..) |it, i| { if (_equal(it.key, _tmp17)) { _found = true; _idx = i; break; } } if (_found) { _tmp16.items[_idx].Items.append(ResultStruct14{ .o = o, .l = l }) catch |err| handleError(err); } else { var g = ResultStruct15{ .key = _tmp17, .Items = std.ArrayList(ResultStruct14).init(std.heap.page_allocator) }; g.Items.append(ResultStruct14{ .o = o, .l = l }) catch |err| handleError(err); _tmp16.append(g) catch |err| handleError(err); } } } var _tmp18 = std.ArrayList(ResultStruct15).init(std.heap.page_allocator);for (_tmp16.items) |g| { _tmp18.append(g) catch |err| handleError(err); } var _tmp19 = std.ArrayList(struct { item: ResultStruct15, key: []const i32 }).init(std.heap.page_allocator);for (_tmp18.items) |g| { _tmp19.append(.{ .item = g, .key = &[_]i32{
    -_sum_int(blk5: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |r| { _tmp12.append((r.l.l_extendedprice * ((1 - r.l.l_discount)))) catch |err| handleError(err); } const _tmp13 = _tmp12.toOwnedSlice() catch |err| handleError(err); break :blk5 _tmp13; }),
    g.key.o_orderdate,
} }) catch |err| handleError(err); } for (0.._tmp19.items.len) |i| { for (i+1.._tmp19.items.len) |j| { if (_tmp19.items[j].key < _tmp19.items[i].key) { const t = _tmp19.items[i]; _tmp19.items[i] = _tmp19.items[j]; _tmp19.items[j] = t; } } } var _tmp20 = std.ArrayList(ResultStruct15).init(std.heap.page_allocator);for (_tmp19.items) |p| { _tmp20.append(p.item) catch |err| handleError(err); } var _tmp21 = std.ArrayList(struct {
    l_orderkey: i32,
    revenue: i32,
    o_orderdate: []const u8,
    o_shippriority: i32,
}).init(std.heap.page_allocator);for (_tmp20.items) |g| { _tmp21.append(OrderLineJoinItem{
    .l_orderkey = g.key.o_orderkey,
    .revenue = _sum_int(blk4: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |r| { _tmp10.append((r.l.l_extendedprice * ((1 - r.l.l_discount)))) catch |err| handleError(err); } const _tmp11 = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp11; }),
    .o_orderdate = g.key.o_orderdate,
    .o_shippriority = g.key.o_shippriority,
}) catch |err| handleError(err); } const _tmp21Slice = _tmp21.toOwnedSlice() catch |err| handleError(err); break :blk6 _tmp21Slice; };
    _json(order_line_join);
    test_Q3_returns_revenue_per_order_with_correct_priority();
}
