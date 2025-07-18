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

fn _max_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var m: i32 = v[0];
    for (v[1..]) |it| { if (it > m) m = it; }
    return m;
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

const SupplierItem = struct {
    s_suppkey: i32,
    s_name: []const u8,
    s_address: []const u8,
    s_phone: []const u8,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_name = "Best Supplier",
    .s_address = "123 Market St",
    .s_phone = "123-456",
},
    SupplierItem{
    .s_suppkey = 200,
    .s_name = "Second Supplier",
    .s_address = "456 Elm St",
    .s_phone = "987-654",
},
}; // []const SupplierItem
const LineitemItem = struct {
    l_suppkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_shipdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_suppkey = 100,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
    .l_shipdate = "1996-01-15",
},
    LineitemItem{
    .l_suppkey = 100,
    .l_extendedprice = 500.0,
    .l_discount = 0.0,
    .l_shipdate = "1996-03-20",
},
    LineitemItem{
    .l_suppkey = 200,
    .l_extendedprice = 800.0,
    .l_discount = 0.05,
    .l_shipdate = "1995-12-30",
},
}; // []const LineitemItem
const start_date = "1996-01-01"; // []const u8
const end_date = "1996-04-01"; // []const u8
const Revenue0Item = struct {
    supplier_no: i32,
    total_revenue: f64,
};
const ResultStruct5 = struct { key: i32, Items: std.ArrayList(LineitemItem) };
var revenue0: []const Revenue0Item = undefined; // []const Revenue0Item
var revenues: []const i32 = undefined; // []const i32
const max_revenue = _max_int(revenues); // i32
const ResultItem = struct {
    s_suppkey: i32,
    s_name: []const u8,
    s_address: []const u8,
    s_phone: []const u8,
    total_revenue: i32,
};
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q15_returns_top_revenue_supplier_s__for_Q1_1996() void {
    const rev = ((1000.0 * 0.9) + 500.0); // f64
    expect((result == &[_]ResultItem{ResultItem{
    .s_suppkey = 100,
    .s_name = "Best Supplier",
    .s_address = "123 Market St",
    .s_phone = "123-456",
    .total_revenue = rev,
}}));
}

pub fn main() void {
    revenue0 = blk2: { var _tmp6 = std.ArrayList(ResultStruct5).init(std.heap.page_allocator); for (lineitem) |l| { if (!((std.mem.order(u8, l.l_shipdate, start_date) != .lt and std.mem.order(u8, l.l_shipdate, end_date) == .lt))) continue; const _tmp7 = l.l_suppkey; var _found = false; var _idx: usize = 0; for (_tmp6.items, 0..) |it, i| { if (_equal(it.key, _tmp7)) { _found = true; _idx = i; break; } } if (_found) { _tmp6.items[_idx].Items.append(l) catch |err| handleError(err); } else { var g = ResultStruct5{ .key = _tmp7, .Items = std.ArrayList(LineitemItem).init(std.heap.page_allocator) }; g.Items.append(l) catch |err| handleError(err); _tmp6.append(g) catch |err| handleError(err); } } var _tmp8 = std.ArrayList(ResultStruct5).init(std.heap.page_allocator);for (_tmp6.items) |g| { _tmp8.append(g) catch |err| handleError(err); } var _tmp9 = std.ArrayList(Revenue0Item).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp9.append(Revenue0Item{
    .supplier_no = g.key,
    .total_revenue = _sum_float(blk1: { var _tmp3 = std.ArrayList(f64).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp3.append((x.l_extendedprice * ((1 - x.l_discount)))) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }),
}) catch |err| handleError(err); } const _tmp9Slice = _tmp9.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp9Slice; };
    revenues = blk3: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (revenue0) |x| { _tmp10.append(x.total_revenue) catch |err| handleError(err); } const _tmp11 = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp11; };
    result = blk4: { var _tmp13 = std.ArrayList(ResultItem).init(std.heap.page_allocator); for (supplier) |s| { for (revenue0) |r| { if (!((s.s_suppkey == r.supplier_no))) continue; if (!((r.total_revenue == max_revenue))) continue; _tmp13.append(ResultItem{
    .s_suppkey = s.s_suppkey,
    .s_name = s.s_name,
    .s_address = s.s_address,
    .s_phone = s.s_phone,
    .total_revenue = r.total_revenue,
}) catch |err| handleError(err); } } const _tmp14 = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp14; };
    _json(result);
    test_Q15_returns_top_revenue_supplier_s__for_Q1_1996();
}
