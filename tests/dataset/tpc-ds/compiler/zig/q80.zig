const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_float(v: []const f64) f64 {
    var sum: f64 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

var store_sales: []const std.AutoHashMap([]const u8, f64) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, f64) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, f64) = undefined;
var total_profit: f64 = undefined;

fn test_TPCDS_Q80_sample() void {
    expect((total_profit == 80));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, f64){blk0: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 20) catch unreachable; m.put("ret", 5) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 10) catch unreachable; m.put("ret", 2) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 5) catch unreachable; m.put("ret", 0) catch unreachable; break :blk2 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, f64){blk3: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 15) catch unreachable; m.put("ret", 3) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 8) catch unreachable; m.put("ret", 1) catch unreachable; break :blk4 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, f64){blk5: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 25) catch unreachable; m.put("ret", 5) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 15) catch unreachable; m.put("ret", 8) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 8) catch unreachable; m.put("ret", 2) catch unreachable; break :blk7 m; }};
    total_profit = ((_sum_float(blk8: { var _tmp0 = std.ArrayList(f64).init(std.heap.page_allocator); for (store_sales) |s| { _tmp0.append((s.price - s.ret)) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk8 _tmp1; }) + _sum_float(blk9: { var _tmp2 = std.ArrayList(f64).init(std.heap.page_allocator); for (catalog_sales) |c| { _tmp2.append((c.price - c.ret)) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk9 _tmp3; })) + _sum_float(blk10: { var _tmp4 = std.ArrayList(f64).init(std.heap.page_allocator); for (web_sales) |w| { _tmp4.append((w.price - w.ret)) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk10 _tmp5; }));
    _json(total_profit);
    test_TPCDS_Q80_sample();
}
