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

const LineitemItem = struct {
    l_extendedprice: f64,
    l_discount: f64,
    l_shipdate: []const u8,
    l_quantity: i32,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_extendedprice = 1000.0,
    .l_discount = 0.06,
    .l_shipdate = "1994-02-15",
    .l_quantity = 10,
},
    LineitemItem{
    .l_extendedprice = 500.0,
    .l_discount = 0.07,
    .l_shipdate = "1994-03-10",
    .l_quantity = 23,
},
    LineitemItem{
    .l_extendedprice = 400.0,
    .l_discount = 0.04,
    .l_shipdate = "1994-04-10",
    .l_quantity = 15,
},
    LineitemItem{
    .l_extendedprice = 200.0,
    .l_discount = 0.06,
    .l_shipdate = "1995-01-01",
    .l_quantity = 5,
},
}; // []const LineitemItem
var result: f64 = undefined; // f64

fn test_Q6_calculates_revenue_from_qualified_lineitems() void {
    expect((result == ((((1000.0 * 0.06)) + ((500.0 * 0.07))))));
}

pub fn main() void {
    result = blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (lineitem) |l| { if (!((((((std.mem.order(u8, l.l_shipdate, "1994-01-01") != .lt) and (std.mem.order(u8, l.l_shipdate, "1995-01-01") == .lt)) and ((l.l_discount >= 0.05))) and ((l.l_discount <= 0.07))) and ((l.l_quantity < 24))))) continue; _tmp0.append(_sum_int((l.l_extendedprice * l.l_discount))) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    _json(result);
    test_Q6_calculates_revenue_from_qualified_lineitems();
}
