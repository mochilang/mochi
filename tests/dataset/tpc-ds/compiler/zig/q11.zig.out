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

const Customer = struct {
    c_customer_sk: i32,
    c_customer_id: []const u8,
    c_first_name: []const u8,
    c_last_name: []const u8,
};

const StoreSale = struct {
    ss_customer_sk: i32,
    ss_sold_date_sk: i32,
    ss_ext_list_price: f64,
};

const WebSale = struct {
    ws_bill_customer_sk: i32,
    ws_sold_date_sk: i32,
    ws_ext_list_price: f64,
};

var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var ss98: f64 = undefined;
var ss99: f64 = undefined;
var ws98: f64 = undefined;
var ws99: f64 = undefined;
var growth_ok: bool = undefined;
var result: []const std.AutoHashMap([]const u8, []const u8) = undefined;

fn test_TPCDS_Q11_growth() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("customer_id", "C1") catch unreachable; m.put("customer_first_name", "John") catch unreachable; m.put("customer_last_name", "Doe") catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    customer = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_customer_id", "C1") catch unreachable; m.put("c_first_name", "John") catch unreachable; m.put("c_last_name", "Doe") catch unreachable; break :blk1 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1998))) catch unreachable; m.put("ss_ext_list_price", 60) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1999))) catch unreachable; m.put("ss_ext_list_price", 90) catch unreachable; break :blk3 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1998))) catch unreachable; m.put("ws_ext_list_price", 50) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1999))) catch unreachable; m.put("ws_ext_list_price", 150) catch unreachable; break :blk5 m; }};
    ss98 = _sum_int(blk6: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |ss| { if (!((ss.ss_sold_date_sk == @as(i32,@intCast(1998))))) continue; _tmp0.append(ss.ss_ext_list_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk6 _tmp1; });
    ss99 = _sum_int(blk7: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |ss| { if (!((ss.ss_sold_date_sk == @as(i32,@intCast(1999))))) continue; _tmp2.append(ss.ss_ext_list_price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk7 _tmp3; });
    ws98 = _sum_int(blk8: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |ws| { if (!((ws.ws_sold_date_sk == @as(i32,@intCast(1998))))) continue; _tmp4.append(ws.ws_ext_list_price) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk8 _tmp5; });
    ws99 = _sum_int(blk9: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |ws| { if (!((ws.ws_sold_date_sk == @as(i32,@intCast(1999))))) continue; _tmp6.append(ws.ws_ext_list_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk9 _tmp7; });
    growth_ok = (((ws98 > @as(i32,@intCast(0))) and (ss98 > @as(i32,@intCast(0)))) and (((ws99 / ws98)) > ((ss99 / ss98))));
    result = if (growth_ok) (&[_]std.AutoHashMap([]const u8, []const u8){blk10: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("customer_id", "C1") catch unreachable; m.put("customer_first_name", "John") catch unreachable; m.put("customer_last_name", "Doe") catch unreachable; break :blk10 m; }}) else (&[_]i32{});
    _json(result);
    test_TPCDS_Q11_growth();
}
