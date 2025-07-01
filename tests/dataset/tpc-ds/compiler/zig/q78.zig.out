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

var ss: []const std.AutoHashMap([]const u8, i32) = undefined;
var ws: []const std.AutoHashMap([]const u8, i32) = undefined;
var cs: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q78_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_year", @as(i32,@intCast(1998))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ratio", 1.25) catch unreachable; m.put("store_qty", @as(i32,@intCast(10))) catch unreachable; m.put("store_wholesale_cost", 50) catch unreachable; m.put("store_sales_price", 100) catch unreachable; m.put("other_chan_qty", @as(i32,@intCast(8))) catch unreachable; m.put("other_chan_wholesale_cost", 40) catch unreachable; m.put("other_chan_sales_price", 80) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    ss = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_year", @as(i32,@intCast(1998))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_qty", @as(i32,@intCast(10))) catch unreachable; m.put("ss_wc", 50) catch unreachable; m.put("ss_sp", 100) catch unreachable; break :blk1 m; }};
    ws = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_year", @as(i32,@intCast(1998))) catch unreachable; m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_qty", @as(i32,@intCast(5))) catch unreachable; m.put("ws_wc", 25) catch unreachable; m.put("ws_sp", 50) catch unreachable; break :blk2 m; }};
    cs = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_year", @as(i32,@intCast(1998))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_qty", @as(i32,@intCast(3))) catch unreachable; m.put("cs_wc", 15) catch unreachable; m.put("cs_sp", 30) catch unreachable; break :blk3 m; }};
    result = blk5: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (ss) |s| { for (ws) |w| { if (!((((w.ws_sold_year == s.ss_sold_year) and (w.ws_item_sk == s.ss_item_sk)) and (w.ws_customer_sk == s.ss_customer_sk)))) continue; for (cs) |c| { if (!((((c.cs_sold_year == s.ss_sold_year) and (c.cs_item_sk == s.ss_item_sk)) and (c.cs_customer_sk == s.ss_customer_sk)))) continue; if (!((((((if ((w == 0)) (@as(i32,@intCast(0))) else (w.ws_qty)) > @as(i32,@intCast(0))) or ((if ((c == 0)) (@as(i32,@intCast(0))) else (c.cs_qty)) > @as(i32,@intCast(0))))) and (s.ss_sold_year == @as(i32,@intCast(1998)))))) continue; _tmp0.append(blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_year", s.ss_sold_year) catch unreachable; m.put("ss_item_sk", s.ss_item_sk) catch unreachable; m.put("ss_customer_sk", s.ss_customer_sk) catch unreachable; m.put("ratio", (s.ss_qty / (((if ((w == 0)) (@as(i32,@intCast(0))) else (w.ws_qty)) + (if ((c == 0)) (@as(i32,@intCast(0))) else (c.cs_qty)))))) catch unreachable; m.put("store_qty", s.ss_qty) catch unreachable; m.put("store_wholesale_cost", s.ss_wc) catch unreachable; m.put("store_sales_price", s.ss_sp) catch unreachable; m.put("other_chan_qty", ((if ((w == 0)) (@as(i32,@intCast(0))) else (w.ws_qty)) + (if ((c == 0)) (@as(i32,@intCast(0))) else (c.cs_qty)))) catch unreachable; m.put("other_chan_wholesale_cost", ((if ((w == 0)) (0) else (w.ws_wc)) + (if ((c == 0)) (0) else (c.cs_wc)))) catch unreachable; m.put("other_chan_sales_price", ((if ((w == 0)) (0) else (w.ws_sp)) + (if ((c == 0)) (0) else (c.cs_sp)))) catch unreachable; break :blk4 m; }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk5 _tmp1; };
    _json(result);
    test_TPCDS_Q78_simplified();
}
