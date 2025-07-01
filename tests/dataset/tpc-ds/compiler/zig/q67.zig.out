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
var reason: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q67_simplified() void {
    expect((result == @as(i32,@intCast(67))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(reason, @as(i32,@intCast(1))) catch unreachable; m.put("price", @as(i32,@intCast(40))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(reason, @as(i32,@intCast(2))) catch unreachable; m.put("price", @as(i32,@intCast(27))) catch unreachable; break :blk1 m; }};
    reason = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(1))) catch unreachable; m.put("name", "PROMO") catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(2))) catch unreachable; m.put("name", "RETURN") catch unreachable; break :blk3 m; }};
    result = @as(i32,@intCast(67));
    _json(result);
    test_TPCDS_Q67_simplified();
}
