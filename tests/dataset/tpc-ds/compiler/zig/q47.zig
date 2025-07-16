const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

var v2: []const std.AutoHashMap([]const u8, i32) = undefined;
var year: i32 = undefined;
var orderby: []const u8 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn abs(x: f64) f64 {
    if (("x" >= 0)) {
        "x";
    } else {
        -"x";
    }
}

fn test_TPCDS_Q47_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2019))) catch unreachable; m.put("item", "C") catch unreachable; m.put("avg_monthly_sales", 50) catch unreachable; m.put("sum_sales", 60) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; m.put("item", "A") catch unreachable; m.put("avg_monthly_sales", 100) catch unreachable; m.put("sum_sales", 120) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    v2 = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; m.put("item", "A") catch unreachable; m.put("avg_monthly_sales", 100) catch unreachable; m.put("sum_sales", 120) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; m.put("item", "B") catch unreachable; m.put("avg_monthly_sales", 80) catch unreachable; m.put("sum_sales", 70) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2019))) catch unreachable; m.put("item", "C") catch unreachable; m.put("avg_monthly_sales", 50) catch unreachable; m.put("sum_sales", 60) catch unreachable; break :blk4 m; }};
    year = @as(i32,@intCast(2020));
    orderby = "item";
    result = blk5: { var _tmp0 = std.ArrayList(struct { item: std.AutoHashMap([]const u8, i32), key: []const i32 }).init(std.heap.page_allocator); for (v2) |v| { if (!((((v.d_year == year) and (v.avg_monthly_sales > @as(i32,@intCast(0)))) and ((abs((v.sum_sales - v.avg_monthly_sales)) / v.avg_monthly_sales) > 0.1)))) continue; _tmp0.append(.{ .item = v, .key = &[_]i32{(v.sum_sales - v.avg_monthly_sales), v.item} }) catch unreachable; } for (0.._tmp0.items.len) |i| { for (i+1.._tmp0.items.len) |j| { if (_tmp0.items[j].key < _tmp0.items[i].key) { const t = _tmp0.items[i]; _tmp0.items[i] = _tmp0.items[j]; _tmp0.items[j] = t; } } } var _tmp1 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp0.items) |p| { _tmp1.append(p.item) catch unreachable; } const _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk5 _tmp2; };
    _json(result);
    test_TPCDS_Q47_simplified();
}
