const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += @floatFromInt(it); }
    return sum / @as(f64, @floatFromInt(v.len));
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

const WebSale = struct {
    ws_item_sk: i32,
    ws_sold_date_sk: i32,
    ws_ext_discount_amt: f64,
};

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var sum_amt: f64 = undefined;
var avg_amt: f64 = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q92_threshold() void {
    expect((result == 4));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_discount_amt", 1) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_discount_amt", 1) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_discount_amt", 2) catch unreachable; break :blk2 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2000-01-02") catch unreachable; break :blk4 m; }};
    sum_amt = _sum_int(blk5: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |ws| { _tmp0.append(ws.ws_ext_discount_amt) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk5 _tmp1; });
    avg_amt = _avg_int(blk6: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |ws| { _tmp2.append(ws.ws_ext_discount_amt) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk6 _tmp3; });
    result = if ((sum_amt > (avg_amt * 1.3))) (sum_amt) else (0);
    _json(result);
    test_TPCDS_Q92_threshold();
}
