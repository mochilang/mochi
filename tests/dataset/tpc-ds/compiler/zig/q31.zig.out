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

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var counties: []const []const u8 = undefined;
var result: []const i32 = undefined;

fn test_TPCDS_Q31_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("web_q1_q2_increase", 1.5) catch unreachable; m.put("store_q1_q2_increase", 1.2) catch unreachable; m.put("web_q2_q3_increase", 1.6666666666666667) catch unreachable; m.put("store_q2_q3_increase", 1.3333333333333333) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_qoy", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ss_ext_sales_price", 120) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_qoy", @as(i32,@intCast(3))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ss_ext_sales_price", 160) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "B") catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ss_ext_sales_price", 80) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "B") catch unreachable; m.put("d_qoy", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ss_ext_sales_price", 90) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "B") catch unreachable; m.put("d_qoy", @as(i32,@intCast(3))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; break :blk6 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ws_ext_sales_price", 100) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_qoy", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ws_ext_sales_price", 150) catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "A") catch unreachable; m.put("d_qoy", @as(i32,@intCast(3))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ws_ext_sales_price", 250) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "B") catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ws_ext_sales_price", 80) catch unreachable; break :blk10 m; }, blk11: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "B") catch unreachable; m.put("d_qoy", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ws_ext_sales_price", 90) catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_county", "B") catch unreachable; m.put("d_qoy", @as(i32,@intCast(3))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("ws_ext_sales_price", 95) catch unreachable; break :blk12 m; }};
    counties = &[_][]const u8{"A", "B"};
    result = &[_]i32{};
    for (counties) |county| {
        const ss1: f64 = _sum_int(blk13: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ca_county == "county") and (s.d_qoy == @as(i32,@intCast(1)))))) continue; _tmp0.append(s.ss_ext_sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk13 _tmp1; });
        const ss2: f64 = _sum_int(blk14: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ca_county == "county") and (s.d_qoy == @as(i32,@intCast(2)))))) continue; _tmp2.append(s.ss_ext_sales_price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk14 _tmp3; });
        const ss3: f64 = _sum_int(blk15: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ca_county == "county") and (s.d_qoy == @as(i32,@intCast(3)))))) continue; _tmp4.append(s.ss_ext_sales_price) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk15 _tmp5; });
        const ws1: f64 = _sum_int(blk16: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |w| { if (!(((w.ca_county == "county") and (w.d_qoy == @as(i32,@intCast(1)))))) continue; _tmp6.append(w.ws_ext_sales_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk16 _tmp7; });
        const ws2: f64 = _sum_int(blk17: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |w| { if (!(((w.ca_county == "county") and (w.d_qoy == @as(i32,@intCast(2)))))) continue; _tmp8.append(w.ws_ext_sales_price) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk17 _tmp9; });
        const ws3: f64 = _sum_int(blk18: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |w| { if (!(((w.ca_county == "county") and (w.d_qoy == @as(i32,@intCast(3)))))) continue; _tmp10.append(w.ws_ext_sales_price) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk18 _tmp11; });
        const web_g1: f64 = (ws2 / ws1);
        const store_g1: f64 = (ss2 / ss1);
        const web_g2: f64 = (ws3 / ws2);
        const store_g2: f64 = (ss3 / ss2);
        if (((web_g1 > store_g1) and (web_g2 > store_g2))) {
            result = append(result, blk19: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_county", "county") catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("web_q1_q2_increase", web_g1) catch unreachable; m.put("store_q1_q2_increase", store_g1) catch unreachable; m.put("web_q2_q3_increase", web_g2) catch unreachable; m.put("store_q2_q3_increase", store_g2) catch unreachable; break :blk19 m; });
        }
    }
    _json(result);
    test_TPCDS_Q31_simplified();
}
