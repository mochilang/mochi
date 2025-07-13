const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _avg_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return @divTrunc(sum, @as(i32, @intCast(v.len)));
}

fn _avg_float(v: []const f64) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += it; }
    return sum / @as(f64, @floatFromInt(v.len));
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _sum_float(v: []const f64) f64 {
    var sum: f64 = 0;
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

const LineitemItem = struct {
    l_quantity: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_tax: f64,
    l_returnflag: []const u8,
    l_linestatus: []const u8,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_quantity = 17,
    .l_extendedprice = 1000.0,
    .l_discount = 0.05,
    .l_tax = 0.07,
    .l_returnflag = "N",
    .l_linestatus = "O",
    .l_shipdate = "1998-08-01",
},
    LineitemItem{
    .l_quantity = 36,
    .l_extendedprice = 2000.0,
    .l_discount = 0.1,
    .l_tax = 0.05,
    .l_returnflag = "N",
    .l_linestatus = "O",
    .l_shipdate = "1998-09-01",
},
    LineitemItem{
    .l_quantity = 25,
    .l_extendedprice = 1500.0,
    .l_discount = 0.0,
    .l_tax = 0.08,
    .l_returnflag = "R",
    .l_linestatus = "F",
    .l_shipdate = "1998-09-03",
},
}; // []const LineitemItem
const ResultStruct0 = struct {
    returnflag: []const u8,
    linestatus: []const u8,
};
const ResultStruct1 = struct {
    returnflag: i32,
    linestatus: i32,
    sum_qty: i32,
    sum_base_price: i32,
    sum_disc_price: i32,
    sum_charge: i32,
    avg_qty: i32,
    avg_price: i32,
    avg_disc: i32,
    count_order: i32,
};
const ResultStruct30 = struct { key: ResultStruct0, Items: std.ArrayList(LineitemItem) };
var result: []const std.StringHashMap(i32) = undefined; // []const std.StringHashMap(i32)

fn test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus() void {
    expect((result == &[_]ResultStruct1{ResultStruct1{
    .returnflag = "N",
    .linestatus = "O",
    .sum_qty = 53,
    .sum_base_price = 3000,
    .sum_disc_price = (950.0 + 1800.0),
    .sum_charge = (((950.0 * 1.07)) + ((1800.0 * 1.05))),
    .avg_qty = 26.5,
    .avg_price = 1500,
    .avg_disc = 0.07500000000000001,
    .count_order = 2,
}}));
}

pub fn main() void {
    result = blk14: { var _tmp31 = std.ArrayList(ResultStruct30).init(std.heap.page_allocator); for (lineitem) |row| { if (!(std.mem.order(u8, row.l_shipdate, "1998-09-02") != .gt)) continue; const _tmp32 = ResultStruct0{
    .returnflag = row.l_returnflag,
    .linestatus = row.l_linestatus,
}; var _found = false; var _idx: usize = 0; for (_tmp31.items, 0..) |it, i| { if (_equal(it.key, _tmp32)) { _found = true; _idx = i; break; } } if (_found) { _tmp31.items[_idx].Items.append(row) catch |err| handleError(err); } else { var g = ResultStruct30{ .key = _tmp32, .Items = std.ArrayList(LineitemItem).init(std.heap.page_allocator) }; g.Items.append(row) catch |err| handleError(err); _tmp31.append(g) catch |err| handleError(err); } } var _tmp33 = std.ArrayList(ResultStruct30).init(std.heap.page_allocator);for (_tmp31.items) |g| { _tmp33.append(g) catch |err| handleError(err); } var _tmp34 = std.ArrayList(struct {
    returnflag: []const u8,
    linestatus: []const u8,
    sum_qty: f64,
    sum_base_price: f64,
    sum_disc_price: f64,
    sum_charge: f64,
    avg_qty: f64,
    avg_price: f64,
    avg_disc: f64,
    count_order: i32,
}).init(std.heap.page_allocator);for (_tmp33.items) |g| { _tmp34.append(ResultStruct1{
    .returnflag = g.key.returnflag,
    .linestatus = g.key.linestatus,
    .sum_qty = _sum_int(blk7: { var _tmp16 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp16.append(x.l_quantity) catch |err| handleError(err); } const _tmp17 = _tmp16.toOwnedSlice() catch |err| handleError(err); break :blk7 _tmp17; }),
    .sum_base_price = _sum_float(blk8: { var _tmp18 = std.ArrayList(f64).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp18.append(x.l_extendedprice) catch |err| handleError(err); } const _tmp19 = _tmp18.toOwnedSlice() catch |err| handleError(err); break :blk8 _tmp19; }),
    .sum_disc_price = _sum_float(blk9: { var _tmp20 = std.ArrayList(f64).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp20.append((x.l_extendedprice * ((1 - x.l_discount)))) catch |err| handleError(err); } const _tmp21 = _tmp20.toOwnedSlice() catch |err| handleError(err); break :blk9 _tmp21; }),
    .sum_charge = _sum_float(blk10: { var _tmp22 = std.ArrayList(f64).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp22.append(((x.l_extendedprice * ((1 - x.l_discount))) * ((1 + x.l_tax)))) catch |err| handleError(err); } const _tmp23 = _tmp22.toOwnedSlice() catch |err| handleError(err); break :blk10 _tmp23; }),
    .avg_qty = _avg_int(blk11: { var _tmp24 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp24.append(x.l_quantity) catch |err| handleError(err); } const _tmp25 = _tmp24.toOwnedSlice() catch |err| handleError(err); break :blk11 _tmp25; }),
    .avg_price = _avg_float(blk12: { var _tmp26 = std.ArrayList(f64).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp26.append(x.l_extendedprice) catch |err| handleError(err); } const _tmp27 = _tmp26.toOwnedSlice() catch |err| handleError(err); break :blk12 _tmp27; }),
    .avg_disc = _avg_float(blk13: { var _tmp28 = std.ArrayList(f64).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp28.append(x.l_discount) catch |err| handleError(err); } const _tmp29 = _tmp28.toOwnedSlice() catch |err| handleError(err); break :blk13 _tmp29; }),
    .count_order = (g.Items.items.len),
}) catch |err| handleError(err); } const _tmp34Slice = _tmp34.toOwnedSlice() catch |err| handleError(err); break :blk14 _tmp34Slice; };
    _json(result);
    test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus();
}
