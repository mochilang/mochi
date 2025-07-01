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

const WebSale = struct {
    ws_sold_time_sk: i32,
    ws_ship_hdemo_sk: i32,
    ws_web_page_sk: i32,
};

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var time_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_page: []const std.AutoHashMap([]const u8, i32) = undefined;
var amc: i32 = undefined;
var pmc: i32 = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q90_ratio() void {
    expect((result == 2));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ship_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_web_page_sk", @as(i32,@intCast(10))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ship_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_web_page_sk", @as(i32,@intCast(10))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_time_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ship_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_web_page_sk", @as(i32,@intCast(10))) catch unreachable; break :blk2 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_dep_count", @as(i32,@intCast(2))) catch unreachable; break :blk3 m; }};
    time_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("t_hour", @as(i32,@intCast(7))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(2))) catch unreachable; m.put("t_hour", @as(i32,@intCast(14))) catch unreachable; break :blk5 m; }};
    web_page = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("wp_web_page_sk", @as(i32,@intCast(10))) catch unreachable; m.put("wp_char_count", @as(i32,@intCast(5100))) catch unreachable; break :blk6 m; }};
    amc = (blk7: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (household_demographics) |hd| { if (!((ws.ws_ship_hdemo_sk == hd.hd_demo_sk))) continue; for (time_dim) |t| { if (!((ws.ws_sold_time_sk == t.t_time_sk))) continue; for (web_page) |wp| { if (!((ws.ws_web_page_sk == wp.wp_web_page_sk))) continue; if (!((((((t.t_hour >= @as(i32,@intCast(7))) and (t.t_hour <= @as(i32,@intCast(8)))) and (hd.hd_dep_count == @as(i32,@intCast(2)))) and (wp.wp_char_count >= @as(i32,@intCast(5000)))) and (wp.wp_char_count <= @as(i32,@intCast(5200)))))) continue; _tmp0.append(ws) catch unreachable; } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; }).len;
    pmc = (blk8: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (household_demographics) |hd| { if (!((ws.ws_ship_hdemo_sk == hd.hd_demo_sk))) continue; for (time_dim) |t| { if (!((ws.ws_sold_time_sk == t.t_time_sk))) continue; for (web_page) |wp| { if (!((ws.ws_web_page_sk == wp.wp_web_page_sk))) continue; if (!((((((t.t_hour >= @as(i32,@intCast(14))) and (t.t_hour <= @as(i32,@intCast(15)))) and (hd.hd_dep_count == @as(i32,@intCast(2)))) and (wp.wp_char_count >= @as(i32,@intCast(5000)))) and (wp.wp_char_count <= @as(i32,@intCast(5200)))))) continue; _tmp2.append(ws) catch unreachable; } } } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk8 _tmp3; }).len;
    result = ((@as(f64, amc)) / (@as(f64, pmc)));
    _json(result);
    test_TPCDS_Q90_ratio();
}
