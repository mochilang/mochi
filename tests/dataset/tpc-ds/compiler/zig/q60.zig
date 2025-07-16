const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains(comptime T: type, v: []const T, item: T) bool {
    for (v) |it| { if (std.meta.eql(it, item)) return true; }
    return false;
}

fn _union_all(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _union(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { if (!_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _except(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (!_contains(T, b, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _intersect(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var all_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q60_simplified() void {
    expect((result == @as(i32,@intCast(60))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(10))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(20))) catch unreachable; break :blk1 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(15))) catch unreachable; break :blk2 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(15))) catch unreachable; break :blk3 m; }};
    all_sales = _union_all(i32, _union_all(std.AutoHashMap([]const u8, i32), store_sales, catalog_sales), web_sales);
    result = _sum_int(blk4: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (all_sales) |s| { _tmp0.append(s.price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk4 _tmp1; });
    _json(result);
    test_TPCDS_Q60_simplified();
}
