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
    l_shipmode: []const u8,
    high_line_count: i32,
    low_line_count: i32,
};
const ResultStruct14 = struct {
    l: LineitemItem,
    o: OrdersItem,
};
const ResultStruct15 = struct { key: []const u8, Items: std.ArrayList(ResultStruct14) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q12_counts_lineitems_by_ship_mode_and_priority() void {
    expect((result == &[_]ResultItem{ResultItem{
    .l_shipmode = "MAIL",
    .high_line_count = 1,
    .low_line_count = 0,
}}));
}

pub fn main() void {
    result = blk9: { var _tmp16 = std.ArrayList(ResultStruct15).init(std.heap.page_allocator); for (lineitem) |l| { for (orders) |o| { if (!((o.o_orderkey == l.l_orderkey))) continue; if (!((((((blk8: { var found: bool = false; for (&[_][]const u8{
    "MAIL",
    "SHIP",
}) |_tmp13| { if (std.mem.eql(u8, _tmp13, l.l_shipmode)) { found = true; break; } } break :blk8 found; }) and (std.mem.order(u8, l.l_commitdate, l.l_receiptdate) == .lt)) and (std.mem.order(u8, l.l_shipdate, l.l_commitdate) == .lt)) and (std.mem.order(u8, l.l_receiptdate, "1994-01-01") != .lt)) and (std.mem.order(u8, l.l_receiptdate, "1995-01-01") == .lt)))) continue; const _tmp17 = l.l_shipmode; var _found = false; var _idx: usize = 0; for (_tmp16.items, 0..) |it, i| { if (_equal(it.key, _tmp17)) { _found = true; _idx = i; break; } } if (_found) { _tmp16.items[_idx].Items.append(ResultStruct14{ .l = l, .o = o }) catch |err| handleError(err); } else { var g = ResultStruct15{ .key = _tmp17, .Items = std.ArrayList(ResultStruct14).init(std.heap.page_allocator) }; g.Items.append(ResultStruct14{ .l = l, .o = o }) catch |err| handleError(err); _tmp16.append(g) catch |err| handleError(err); } } } var _tmp18 = std.ArrayList(ResultStruct15).init(std.heap.page_allocator);for (_tmp16.items) |g| { _tmp18.append(g) catch |err| handleError(err); } var _tmp19 = std.ArrayList(struct { item: ResultStruct15, key: i32 }).init(std.heap.page_allocator);for (_tmp18.items) |g| { _tmp19.append(.{ .item = g, .key = g.key }) catch |err| handleError(err); } for (0.._tmp19.items.len) |i| { for (i+1.._tmp19.items.len) |j| { if (_tmp19.items[j].key < _tmp19.items[i].key) { const t = _tmp19.items[i]; _tmp19.items[i] = _tmp19.items[j]; _tmp19.items[j] = t; } } } var _tmp20 = std.ArrayList(ResultStruct15).init(std.heap.page_allocator);for (_tmp19.items) |p| { _tmp20.append(p.item) catch |err| handleError(err); } var _tmp21 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp20.items) |g| { _tmp21.append(ResultItem{
    .l_shipmode = g.key,
    .high_line_count = _sum_int(blk5: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp8.append(if (blk4: { var found: bool = false; for (&[_][]const u8{
    "1-URGENT",
    "2-HIGH",
}) |_tmp7| { if (std.mem.eql(u8, _tmp7, x.o.o_orderpriority)) { found = true; break; } } break :blk4 found; }) (1) else (0)) catch |err| handleError(err); } const _tmp9 = _tmp8.toOwnedSlice() catch |err| handleError(err); break :blk5 _tmp9; }),
    .low_line_count = _sum_int(blk7: { var _tmp11 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp11.append(if (!(blk6: { var found: bool = false; for (&[_][]const u8{
    "1-URGENT",
    "2-HIGH",
}) |_tmp10| { if (std.mem.eql(u8, _tmp10, x.o.o_orderpriority)) { found = true; break; } } break :blk6 found; })) (1) else (0)) catch |err| handleError(err); } const _tmp12 = _tmp11.toOwnedSlice() catch |err| handleError(err); break :blk7 _tmp12; }),
}) catch |err| handleError(err); } const _tmp21Slice = _tmp21.toOwnedSlice() catch |err| handleError(err); break :blk9 _tmp21Slice; };
    _json(result);
    test_Q12_counts_lineitems_by_ship_mode_and_priority();
}
