const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q68_simplified() void {
    expect((result == @as(i32,@intCast(68))));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("profit", @as(i32,@intCast(30))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(2))) catch unreachable; m.put("profit", @as(i32,@intCast(38))) catch unreachable; break :blk1 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("profit", @as(i32,@intCast(30))) catch unreachable; break :blk2 m; }};
    result = ((_sum_int(blk3: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (catalog_sales) |c| { _tmp0.append(c.profit) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk3 _tmp1; }) - _sum_int(blk4: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { _tmp2.append(s.profit) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk4 _tmp3; })) + @as(i32,@intCast(30)));
    _json(result);
    test_TPCDS_Q68_simplified();
}
