const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _sum_float(v: []const f64) f64 {
    var sum: f64 = 0;
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

const PartItem = struct {
    p_partkey: i32,
    p_brand: []const u8,
    p_container: []const u8,
    p_size: i32,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 1,
    .p_brand = "Brand#12",
    .p_container = "SM BOX",
    .p_size = 3,
},
    PartItem{
    .p_partkey = 2,
    .p_brand = "Brand#23",
    .p_container = "MED BOX",
    .p_size = 5,
},
    PartItem{
    .p_partkey = 3,
    .p_brand = "Brand#34",
    .p_container = "LG BOX",
    .p_size = 15,
},
}; // []const PartItem
const LineitemItem = struct {
    l_partkey: i32,
    l_quantity: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_shipmode: []const u8,
    l_shipinstruct: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_partkey = 1,
    .l_quantity = 5,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
    .l_shipmode = "AIR",
    .l_shipinstruct = "DELIVER IN PERSON",
},
    LineitemItem{
    .l_partkey = 2,
    .l_quantity = 15,
    .l_extendedprice = 2000.0,
    .l_discount = 0.05,
    .l_shipmode = "AIR REG",
    .l_shipinstruct = "DELIVER IN PERSON",
},
    LineitemItem{
    .l_partkey = 3,
    .l_quantity = 35,
    .l_extendedprice = 1500.0,
    .l_discount = 0.0,
    .l_shipmode = "AIR",
    .l_shipinstruct = "DELIVER IN PERSON",
},
}; // []const LineitemItem
var revenues: []const f64 = undefined; // []const f64
const result = _sum_float(revenues); // f64

fn test_Q19_returns_total_revenue_from_qualifying_branded_parts() void {
    expect((result == 2800.0));
}

pub fn main() void {
    revenues = blk0: { var _tmp0 = std.ArrayList(f64).init(std.heap.page_allocator); for (lineitem) |l| { for (part) |p| { if (!((p.p_partkey == l.l_partkey))) continue; if (!(((((((((((std.mem.eql(u8, p.p_brand, "Brand#12")) and (_contains_list_string(&[_][]const u8{
    "SM CASE",
    "SM BOX",
    "SM PACK",
    "SM PKG",
}, p.p_container))) and (((l.l_quantity >= 1) and (l.l_quantity <= 11)))) and (((p.p_size >= 1) and (p.p_size <= 5))))) or (((((std.mem.eql(u8, p.p_brand, "Brand#23")) and (_contains_list_string(&[_][]const u8{
    "MED BAG",
    "MED BOX",
    "MED PKG",
    "MED PACK",
}, p.p_container))) and (((l.l_quantity >= 10) and (l.l_quantity <= 20)))) and (((p.p_size >= 1) and (p.p_size <= 10)))))) or (((((std.mem.eql(u8, p.p_brand, "Brand#34")) and (_contains_list_string(&[_][]const u8{
    "LG CASE",
    "LG BOX",
    "LG PACK",
    "LG PKG",
}, p.p_container))) and (((l.l_quantity >= 20) and (l.l_quantity <= 30)))) and (((p.p_size >= 1) and (p.p_size <= 15))))))) and _contains_list_string(&[_][]const u8{
    "AIR",
    "AIR REG",
}, l.l_shipmode)) and std.mem.eql(u8, l.l_shipinstruct, "DELIVER IN PERSON")))) continue; _tmp0.append((l.l_extendedprice * ((1 - l.l_discount)))) catch |err| handleError(err); } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    _json(result);
    test_Q19_returns_total_revenue_from_qualifying_branded_parts();
}
