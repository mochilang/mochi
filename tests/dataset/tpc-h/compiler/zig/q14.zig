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

const PartItem = struct {
    p_partkey: i32,
    p_type: []const u8,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 1,
    .p_type = "PROMO LUXURY",
},
    PartItem{
    .p_partkey = 2,
    .p_type = "STANDARD BRASS",
},
}; // []const PartItem
const LineitemItem = struct {
    l_partkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_partkey = 1,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
    .l_shipdate = "1995-09-05",
},
    LineitemItem{
    .l_partkey = 2,
    .l_extendedprice = 800.0,
    .l_discount = 0.0,
    .l_shipdate = "1995-09-20",
},
    LineitemItem{
    .l_partkey = 1,
    .l_extendedprice = 500.0,
    .l_discount = 0.2,
    .l_shipdate = "1995-10-02",
},
}; // []const LineitemItem
const start_date = "1995-09-01"; // []const u8
const end_date = "1995-10-01"; // []const u8
const FilteredItem = struct {
    is_promo: bool,
    revenue: f64,
};
var filtered: []const FilteredItem = undefined; // []const FilteredItem
const promo_sum = _sum_int(blk1: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { if (!(x.is_promo)) continue; _tmp3.append(x.revenue) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }); // f64
const total_sum = _sum_int(blk2: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { _tmp5.append(x.revenue) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; }); // f64
const result = ((100.0 * promo_sum) / total_sum); // f64

fn test_Q14_calculates_promo_revenue_percent_in_1995_09() void {
    const promo = (1000.0 * 0.9); // f64
    const total = (900 + 800.0); // f64
    const expected = ((100.0 * promo) / total); // f64
    expect((result == expected));
}

pub fn main() void {
    filtered = blk0: { var _tmp1 = std.ArrayList(FilteredItem).init(std.heap.page_allocator); for (lineitem) |l| { for (part) |p| { if (!((p.p_partkey == l.l_partkey))) continue; if (!((std.mem.order(u8, l.l_shipdate, start_date) != .lt and std.mem.order(u8, l.l_shipdate, end_date) == .lt))) continue; _tmp1.append(FilteredItem{
    .is_promo = "PROMO",
    .revenue = (l.l_extendedprice * ((1 - l.l_discount))),
}) catch |err| handleError(err); } } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp2; };
    _json(result);
    test_Q14_calculates_promo_revenue_percent_in_1995_09();
}
