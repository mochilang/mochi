const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
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

const OrdersItem = struct {
    o_orderkey: i32,
    o_orderdate: []const u8,
    o_orderpriority: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 1,
    .o_orderdate = "1993-07-01",
    .o_orderpriority = "1-URGENT",
},
    OrdersItem{
    .o_orderkey = 2,
    .o_orderdate = "1993-07-15",
    .o_orderpriority = "2-HIGH",
},
    OrdersItem{
    .o_orderkey = 3,
    .o_orderdate = "1993-08-01",
    .o_orderpriority = "3-NORMAL",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_commitdate: []const u8,
    l_receiptdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1,
    .l_commitdate = "1993-07-10",
    .l_receiptdate = "1993-07-12",
},
    LineitemItem{
    .l_orderkey = 1,
    .l_commitdate = "1993-07-12",
    .l_receiptdate = "1993-07-10",
},
    LineitemItem{
    .l_orderkey = 2,
    .l_commitdate = "1993-07-20",
    .l_receiptdate = "1993-07-25",
},
    LineitemItem{
    .l_orderkey = 3,
    .l_commitdate = "1993-08-02",
    .l_receiptdate = "1993-08-01",
},
    LineitemItem{
    .l_orderkey = 3,
    .l_commitdate = "1993-08-05",
    .l_receiptdate = "1993-08-10",
},
}; // []const LineitemItem
const start_date = "1993-07-01"; // []const u8
const end_date = "1993-08-01"; // []const u8
var date_filtered_orders: []const OrdersItem = undefined; // []const OrdersItem
var late_orders: []const OrdersItem = undefined; // []const OrdersItem
const ResultItem = struct {
    o_orderpriority: []const u8,
    order_count: i32,
};
const ResultStruct7 = struct { key: []const u8, Items: std.ArrayList(OrdersItem) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q4_returns_count_of_orders_with_late_lineitems_in_range() void {
    expect((result == &[_]ResultItem{
    ResultItem{
    .o_orderpriority = "1-URGENT",
    .order_count = 1,
},
    ResultItem{
    .o_orderpriority = "2-HIGH",
    .order_count = 1,
},
}));
}

pub fn main() void {
    date_filtered_orders = blk0: { var _tmp0 = std.ArrayList(OrdersItem).init(std.heap.page_allocator); for (orders) |o| { if (!((std.mem.order(u8, o.o_orderdate, start_date) != .lt and std.mem.order(u8, o.o_orderdate, end_date) == .lt))) continue; _tmp0.append(o) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    late_orders = blk2: { var _tmp4 = std.ArrayList(OrdersItem).init(std.heap.page_allocator); for (date_filtered_orders) |o| { if (!((blk1: { var _tmp2 = std.ArrayList(LineitemItem).init(std.heap.page_allocator); for (lineitem) |l| { if (!(((l.l_orderkey == o.o_orderkey) and std.mem.order(u8, l.l_commitdate, l.l_receiptdate) == .lt))) continue; _tmp2.append(l) catch |err| handleError(err); } const _tmp3 = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp3; }).len != 0)) continue; _tmp4.append(o) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp5; };
    result = blk3: { var _tmp8 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator); for (late_orders) |o| { const _tmp9 = o.o_orderpriority; var _found = false; var _idx: usize = 0; for (_tmp8.items, 0..) |it, i| { if (_equal(it.key, _tmp9)) { _found = true; _idx = i; break; } } if (_found) { _tmp8.items[_idx].Items.append(o) catch |err| handleError(err); } else { var g = ResultStruct7{ .key = _tmp9, .Items = std.ArrayList(OrdersItem).init(std.heap.page_allocator) }; g.Items.append(o) catch |err| handleError(err); _tmp8.append(g) catch |err| handleError(err); } } var _tmp10 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp10.append(g) catch |err| handleError(err); } var _tmp11 = std.ArrayList(struct { item: ResultStruct7, key: i32 }).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(.{ .item = g, .key = g.key }) catch |err| handleError(err); } for (0.._tmp11.items.len) |i| { for (i+1.._tmp11.items.len) |j| { if (_tmp11.items[j].key < _tmp11.items[i].key) { const t = _tmp11.items[i]; _tmp11.items[i] = _tmp11.items[j]; _tmp11.items[j] = t; } } } var _tmp12 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp11.items) |p| { _tmp12.append(p.item) catch |err| handleError(err); } var _tmp13 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(ResultItem{
    .o_orderpriority = g.key,
    .order_count = @as(i32, @intCast(g.Items.items.len)),
}) catch |err| handleError(err); } const _tmp13Slice = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp13Slice; };
    _json(result);
    test_Q4_returns_count_of_orders_with_late_lineitems_in_range();
}
