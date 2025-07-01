const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

const WebSale = struct {
    ws_order_number: i32,
    ws_warehouse_sk: i32,
    ws_ship_date_sk: i32,
    ws_ship_addr_sk: i32,
    ws_web_site_sk: i32,
    ws_ext_ship_cost: f64,
    ws_net_profit: f64,
};

const WebReturn = struct {
    wr_order_number: i32,
};

const DateDim = struct {
    d_date_sk: i32,
    d_date: []const u8,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_state: []const u8,
};

const WebSite = struct {
    web_site_sk: i32,
    web_company_name: []const u8,
};

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_site: []const std.AutoHashMap([]const u8, i32) = undefined;
var ws_wh: []const std.AutoHashMap([]const u8, i32) = undefined;
var filtered: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: std.AutoHashMap([]const u8, i32) = undefined;

fn distinct(xs: []const i32) []const i32 {
    var out = std.ArrayList(i32).init(std.heap.page_allocator);
    for ("xs") |x| {
        if (!contains(out, "x")) {
            out = append(out, "x");
        }
    }
    return out.items;
}

fn test_TPCDS_Q95_shipping_returns() void {
    expect((result == blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order_count", @as(i32,@intCast(1))) catch unreachable; m.put("total_shipping_cost", 2) catch unreachable; m.put("total_net_profit", 5) catch unreachable; break :blk0 m; }));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_order_number", @as(i32,@intCast(1))) catch unreachable; m.put("ws_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ship_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ship_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_web_site_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_ship_cost", 2) catch unreachable; m.put("ws_net_profit", 5) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_order_number", @as(i32,@intCast(1))) catch unreachable; m.put("ws_warehouse_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ship_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ship_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_web_site_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_ship_cost", 0) catch unreachable; m.put("ws_net_profit", 0) catch unreachable; break :blk2 m; }};
    web_returns = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("wr_order_number", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2001-02-01") catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk5 m; }};
    web_site = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("web_site_sk", @as(i32,@intCast(1))) catch unreachable; m.put("web_company_name", "pri") catch unreachable; break :blk6 m; }};
    ws_wh = blk8: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws1| { for (web_sales) |ws2| { if (!(((ws1.ws_order_number == ws2.ws_order_number) and (ws1.ws_warehouse_sk != ws2.ws_warehouse_sk)))) continue; _tmp0.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_order_number", ws1.ws_order_number) catch unreachable; break :blk7 m; }) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk8 _tmp1; };
    filtered = blk11: { var _tmp6 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (date_dim) |d| { if (!((ws.ws_ship_date_sk == d.d_date_sk))) continue; for (customer_address) |ca| { if (!((ws.ws_ship_addr_sk == ca.ca_address_sk))) continue; for (web_site) |w| { if (!((ws.ws_web_site_sk == w.web_site_sk))) continue; if (!((((std.mem.eql(u8, ca.ca_state, "CA") and std.mem.eql(u8, w.web_company_name, "pri")) and _contains_list_int((blk9: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (ws_wh) |x| { _tmp2.append(x.ws_order_number) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk9 _tmp3; }), ws.ws_order_number)) and _contains_list_int((blk10: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_returns) |wr| { _tmp4.append(wr.wr_order_number) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk10 _tmp5; }), ws.ws_order_number)))) continue; _tmp6.append(ws) catch unreachable; } } } } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk11 _tmp7; };
    result = blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order_count", (distinct(blk13: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { _tmp8.append(x.ws_order_number) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk13 _tmp9; })).len) catch unreachable; m.put("total_shipping_cost", _sum_int(blk14: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { _tmp10.append(x.ws_ext_ship_cost) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk14 _tmp11; })) catch unreachable; m.put("total_net_profit", _sum_int(blk15: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |x| { _tmp12.append(x.ws_net_profit) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk15 _tmp13; })) catch unreachable; break :blk12 m; };
    _json(result);
    test_TPCDS_Q95_shipping_returns();
}
