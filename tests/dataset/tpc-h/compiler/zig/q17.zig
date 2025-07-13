const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += @floatFromInt(it); }
    return sum / @as(f64, @floatFromInt(v.len));
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

const PartItem = struct {
    p_partkey: i32,
    p_brand: []const u8,
    p_container: []const u8,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 1,
    .p_brand = "Brand#23",
    .p_container = "MED BOX",
},
    PartItem{
    .p_partkey = 2,
    .p_brand = "Brand#77",
    .p_container = "LG JAR",
},
}; // []const PartItem
const LineitemItem = struct {
    l_partkey: i32,
    l_quantity: i32,
    l_extendedprice: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_partkey = 1,
    .l_quantity = 1,
    .l_extendedprice = 100.0,
},
    LineitemItem{
    .l_partkey = 1,
    .l_quantity = 10,
    .l_extendedprice = 1000.0,
},
    LineitemItem{
    .l_partkey = 1,
    .l_quantity = 20,
    .l_extendedprice = 2000.0,
},
    LineitemItem{
    .l_partkey = 2,
    .l_quantity = 5,
    .l_extendedprice = 500.0,
},
}; // []const LineitemItem
const brand = "Brand#23"; // []const u8
const container = "MED BOX"; // []const u8
var filtered: []const f64 = undefined; // []const f64
const result = (_sum_float(filtered) / 7.0); // f64

fn test_Q17_returns_average_yearly_revenue_for_small_quantity_orders() void {
    const expected = (100.0 / 7.0); // f64
    expect((result == expected));
}

pub fn main() void {
    filtered = blk1: { var _tmp2 = std.ArrayList(f64).init(std.heap.page_allocator); for (lineitem) |l| { for (part) |p| { if (!((p.p_partkey == l.l_partkey))) continue; if (!(((((std.mem.eql(u8, p.p_brand, brand)) and (std.mem.eql(u8, p.p_container, container))) and ((l.l_quantity < ((0.2 * _avg_int(blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (lineitem) |x| { if (!((x.l_partkey == p.p_partkey))) continue; _tmp0.append(x.l_quantity) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }))))))))) continue; _tmp2.append(l.l_extendedprice) catch |err| handleError(err); } } const _tmp3 = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp3; };
    _json(result);
    test_Q17_returns_average_yearly_revenue_for_small_quantity_orders();
}
