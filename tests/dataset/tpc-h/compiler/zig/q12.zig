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

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
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

const OrdersItem = struct {
    o_orderkey: i32,
    o_orderpriority: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 1,
    .o_orderpriority = "1-URGENT",
},
    OrdersItem{
    .o_orderkey = 2,
    .o_orderpriority = "3-MEDIUM",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_shipmode: []const u8,
    l_commitdate: []const u8,
    l_receiptdate: []const u8,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1,
    .l_shipmode = "MAIL",
    .l_commitdate = "1994-02-10",
    .l_receiptdate = "1994-02-15",
    .l_shipdate = "1994-02-05",
},
    LineitemItem{
    .l_orderkey = 2,
    .l_shipmode = "SHIP",
    .l_commitdate = "1994-03-01",
    .l_receiptdate = "1994-02-28",
    .l_shipdate = "1994-02-27",
},
}; // []const LineitemItem
const ResultItem = struct {
    l_shipmode: i32,
    high_line_count: i32,
    low_line_count: i32,
};
const ResultStruct9 = struct {
    l: LineitemItem,
    o: OrdersItem,
};
const ResultStruct10 = struct { key: []const u8, Items: std.ArrayList(ResultStruct9) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q12_counts_lineitems_by_ship_mode_and_priority() void {
    expect((result == &[_]ResultItem{ResultItem{
    .l_shipmode = "MAIL",
    .high_line_count = 1,
    .low_line_count = 0,
}}));
}

pub fn main() void {
    result = blk4: { var _tmp11 = std.ArrayList(ResultStruct10).init(std.heap.page_allocator); for (lineitem) |l| { for (orders) |o| { if (!((o.o_orderkey == l.l_orderkey))) continue; if (!((((((_contains_list_string(&[_][]const u8{
    "MAIL",
    "SHIP",
}, l.l_shipmode)) and (std.mem.order(u8, l.l_commitdate, l.l_receiptdate) == .lt)) and (std.mem.order(u8, l.l_shipdate, l.l_commitdate) == .lt)) and (std.mem.order(u8, l.l_receiptdate, "1994-01-01") != .lt)) and (std.mem.order(u8, l.l_receiptdate, "1995-01-01") == .lt)))) continue; const _tmp12 = l.l_shipmode; var _found = false; var _idx: usize = 0; for (_tmp11.items, 0..) |it, i| { if (_equal(it.key, _tmp12)) { _found = true; _idx = i; break; } } if (_found) { _tmp11.items[_idx].Items.append(ResultStruct9{ .l = l, .o = o }) catch |err| handleError(err); } else { var g = ResultStruct10{ .key = _tmp12, .Items = std.ArrayList(ResultStruct9).init(std.heap.page_allocator) }; g.Items.append(ResultStruct9{ .l = l, .o = o }) catch |err| handleError(err); _tmp11.append(g) catch |err| handleError(err); } } } var _tmp13 = std.ArrayList(ResultStruct10).init(std.heap.page_allocator);for (_tmp11.items) |g| { _tmp13.append(g) catch |err| handleError(err); } var _tmp14 = std.ArrayList(struct { item: ResultStruct10, key: i32 }).init(std.heap.page_allocator);for (_tmp13.items) |g| { _tmp14.append(.{ .item = g, .key = g.key }) catch |err| handleError(err); } for (0.._tmp14.items.len) |i| { for (i+1.._tmp14.items.len) |j| { if (_tmp14.items[j].key < _tmp14.items[i].key) { const t = _tmp14.items[i]; _tmp14.items[i] = _tmp14.items[j]; _tmp14.items[j] = t; } } } var _tmp15 = std.ArrayList(ResultStruct10).init(std.heap.page_allocator);for (_tmp14.items) |p| { _tmp15.append(p.item) catch |err| handleError(err); } var _tmp16 = std.ArrayList(struct {
    l_shipmode: []const u8,
    high_line_count: i32,
    low_line_count: i32,
}).init(std.heap.page_allocator);for (_tmp15.items) |g| { _tmp16.append(ResultItem{
    .l_shipmode = g.key,
    .high_line_count = _sum_int(blk2: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp5.append(if (_contains_list_string(&[_][]const u8{
    "1-URGENT",
    "2-HIGH",
}, x.o.o_orderpriority)) (1) else (0)) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; }),
    .low_line_count = _sum_int(blk3: { var _tmp7 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp7.append(if (!(_contains_list_string(&[_][]const u8{
    "1-URGENT",
    "2-HIGH",
}, x.o.o_orderpriority))) (1) else (0)) catch |err| handleError(err); } const _tmp8 = _tmp7.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp8; }),
}) catch |err| handleError(err); } const _tmp16Slice = _tmp16.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp16Slice; };
    _json(result);
    test_Q12_counts_lineitems_by_ship_mode_and_priority();
}
