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
var result: f64 = undefined;

fn test_TPCDS_Q89_sample() void {
    expect((result == 89));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, f64){blk0: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 40) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 30) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("price", 19) catch unreachable; break :blk2 m; }};
    result = _sum_float(blk3: { var _tmp0 = std.ArrayList(f64).init(std.heap.page_allocator); for (store_sales) |s| { _tmp0.append(s.price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk3 _tmp1; });
    _json(result);
    test_TPCDS_Q89_sample();
}
