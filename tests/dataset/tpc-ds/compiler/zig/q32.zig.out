const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += @floatFromInt(it); }
    return sum / @as(f64, @floatFromInt(v.len));
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var filtered: []const i32 = undefined;
var avg_discount: f64 = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q32_simplified() void {
    expect((result == 20));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ext_discount_amt", 5) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_ext_discount_amt", 10) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cs_ext_discount_amt", 20) catch unreachable; break :blk2 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk6 m; }};
    filtered = blk7: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (item) |i| { if (!((cs.cs_item_sk == i.i_item_sk))) continue; for (date_dim) |d| { if (!((cs.cs_sold_date_sk == d.d_date_sk))) continue; if (!(((i.i_manufact_id == @as(i32,@intCast(1))) and (d.d_year == @as(i32,@intCast(2000)))))) continue; _tmp0.append(cs.cs_ext_discount_amt) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; };
    avg_discount = _avg_int(filtered);
    result = _sum_int(blk8: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { if (!((x > (avg_discount * 1.3)))) continue; _tmp2.append(x) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk8 _tmp3; });
    _json(result);
    test_TPCDS_Q32_simplified();
}
