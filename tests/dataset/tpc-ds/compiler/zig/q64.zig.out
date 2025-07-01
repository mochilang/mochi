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
var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q64_simplified() void {
    expect((result == @as(i32,@intCast(64))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("cost", @as(i32,@intCast(20))) catch unreachable; m.put("list", @as(i32,@intCast(30))) catch unreachable; m.put("coupon", @as(i32,@intCast(5))) catch unreachable; break :blk0 m; }};
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item", @as(i32,@intCast(1))) catch unreachable; m.put("ticket", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    result = (((@as(i32,@intCast(20)) + @as(i32,@intCast(30))) - @as(i32,@intCast(5))) + @as(i32,@intCast(19)));
    _json(result);
    test_TPCDS_Q64_simplified();
}
