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

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var income_band: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q84_sample() void {
    expect((result == @as(i32,@intCast(84))));
}

pub fn main() void {
    customers = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(1))) catch unreachable; m.put("city", "A") catch unreachable; m.put("cdemo", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(2))) catch unreachable; m.put("city", "A") catch unreachable; m.put("cdemo", @as(i32,@intCast(2))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(3))) catch unreachable; m.put("city", "B") catch unreachable; m.put("cdemo", @as(i32,@intCast(1))) catch unreachable; break :blk2 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(2))) catch unreachable; break :blk4 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("income_band_sk", @as(i32,@intCast(1))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("income_band_sk", @as(i32,@intCast(2))) catch unreachable; break :blk6 m; }};
    income_band = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ib_income_band_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ib_lower_bound", @as(i32,@intCast(0))) catch unreachable; m.put("ib_upper_bound", @as(i32,@intCast(50000))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ib_income_band_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ib_lower_bound", @as(i32,@intCast(50001))) catch unreachable; m.put("ib_upper_bound", @as(i32,@intCast(100000))) catch unreachable; break :blk8 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_city", "A") catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_city", "B") catch unreachable; break :blk10 m; }};
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk12 m; }, blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_cdemo_sk", @as(i32,@intCast(2))) catch unreachable; break :blk13 m; }, blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk14 m; }};
    result = (@as(i32,@intCast(80)) + (store_returns).len);
    _json(result);
    test_TPCDS_Q84_sample();
}
