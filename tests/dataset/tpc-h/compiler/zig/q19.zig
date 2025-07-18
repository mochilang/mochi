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
    revenues = blk4: { var _tmp4 = std.ArrayList(f64).init(std.heap.page_allocator); for (lineitem) |l| { for (part) |p| { if (!((p.p_partkey == l.l_partkey))) continue; if (!(((((((((((std.mem.eql(u8, p.p_brand, "Brand#12")) and (blk0: { var found: bool = false; for (&[_][]const u8{
    "SM CASE",
    "SM BOX",
    "SM PACK",
    "SM PKG",
}) |_tmp0| { if (std.mem.eql(u8, _tmp0, p.p_container)) { found = true; break; } } break :blk0 found; })) and (((l.l_quantity >= 1) and (l.l_quantity <= 11)))) and (((p.p_size >= 1) and (p.p_size <= 5))))) or (((((std.mem.eql(u8, p.p_brand, "Brand#23")) and (blk1: { var found: bool = false; for (&[_][]const u8{
    "MED BAG",
    "MED BOX",
    "MED PKG",
    "MED PACK",
}) |_tmp1| { if (std.mem.eql(u8, _tmp1, p.p_container)) { found = true; break; } } break :blk1 found; })) and (((l.l_quantity >= 10) and (l.l_quantity <= 20)))) and (((p.p_size >= 1) and (p.p_size <= 10)))))) or (((((std.mem.eql(u8, p.p_brand, "Brand#34")) and (blk2: { var found: bool = false; for (&[_][]const u8{
    "LG CASE",
    "LG BOX",
    "LG PACK",
    "LG PKG",
}) |_tmp2| { if (std.mem.eql(u8, _tmp2, p.p_container)) { found = true; break; } } break :blk2 found; })) and (((l.l_quantity >= 20) and (l.l_quantity <= 30)))) and (((p.p_size >= 1) and (p.p_size <= 15))))))) and blk3: { var found: bool = false; for (&[_][]const u8{
    "AIR",
    "AIR REG",
}) |_tmp3| { if (std.mem.eql(u8, _tmp3, l.l_shipmode)) { found = true; break; } } break :blk3 found; }) and std.mem.eql(u8, l.l_shipinstruct, "DELIVER IN PERSON")))) continue; _tmp4.append((l.l_extendedprice * ((1 - l.l_discount)))) catch |err| handleError(err); } } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp5; };
    _json(result);
    test_Q19_returns_total_revenue_from_qualifying_branded_parts();
}
