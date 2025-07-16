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

var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var inventory: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q82_sample() void {
    expect((result == @as(i32,@intCast(82))));
}

pub fn main() void {
    item = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(2))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(3))) catch unreachable; break :blk2 m; }};
    inventory = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(1))) catch unreachable; m.put("qty", @as(i32,@intCast(20))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(1))) catch unreachable; m.put("qty", @as(i32,@intCast(22))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(1))) catch unreachable; m.put("qty", @as(i32,@intCast(5))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(2))) catch unreachable; m.put("qty", @as(i32,@intCast(30))) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(2))) catch unreachable; m.put("qty", @as(i32,@intCast(5))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(3))) catch unreachable; m.put("qty", @as(i32,@intCast(10))) catch unreachable; break :blk8 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(item, @as(i32,@intCast(2))) catch unreachable; break :blk10 m; }};
    result = @as(i32,@intCast(0));
    for (inventory) |inv| {
        for (store_sales) |s| {
            if ((inv.item == s.item)) {
                result = (result + inv.qty);
            }
        }
    }
    _json(result);
    test_TPCDS_Q82_sample();
}
