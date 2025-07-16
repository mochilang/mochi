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

var time_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q88_sample() void {
    expect((result == @as(i32,@intCast(88))));
}

pub fn main() void {
    time_dim = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hour", @as(i32,@intCast(8))) catch unreachable; m.put("minute", @as(i32,@intCast(30))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("time_sk", @as(i32,@intCast(2))) catch unreachable; m.put("hour", @as(i32,@intCast(9))) catch unreachable; m.put("minute", @as(i32,@intCast(0))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("time_sk", @as(i32,@intCast(3))) catch unreachable; m.put("hour", @as(i32,@intCast(11))) catch unreachable; m.put("minute", @as(i32,@intCast(15))) catch unreachable; break :blk2 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_time_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_time_sk", @as(i32,@intCast(2))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_time_sk", @as(i32,@intCast(3))) catch unreachable; break :blk5 m; }};
    result = @as(i32,@intCast(88));
    _json(result);
    test_TPCDS_Q88_sample();
}
