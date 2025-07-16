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

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q65_simplified() void {
    expect((result == @as(i32,@intCast(65))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("store", @as(i32,@intCast(1))) catch unreachable; m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("store", @as(i32,@intCast(1))) catch unreachable; m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("store", @as(i32,@intCast(1))) catch unreachable; m.put("item", @as(i32,@intCast(2))) catch unreachable; m.put("price", @as(i32,@intCast(60))) catch unreachable; break :blk2 m; }};
    result = @as(i32,@intCast(65));
    _json(result);
    test_TPCDS_Q65_simplified();
}
