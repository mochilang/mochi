const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
    return false;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var year: i32 = undefined;
var states1: []const []const u8 = undefined;
var states2: []const []const u8 = undefined;
var states3: []const []const u8 = undefined;
var qty_base: []const i32 = undefined;
var qty: []const i32 = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q48_simplified() void {
    expect((result == @as(i32,@intCast(35))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 120) catch unreachable; m.put("net_profit", 1000) catch unreachable; m.put("quantity", @as(i32,@intCast(5))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cdemo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("addr_sk", @as(i32,@intCast(2))) catch unreachable; m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 60) catch unreachable; m.put("net_profit", 2000) catch unreachable; m.put("quantity", @as(i32,@intCast(10))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cdemo_sk", @as(i32,@intCast(3))) catch unreachable; m.put("addr_sk", @as(i32,@intCast(3))) catch unreachable; m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 170) catch unreachable; m.put("net_profit", 10000) catch unreachable; m.put("quantity", @as(i32,@intCast(20))) catch unreachable; break :blk2 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_marital_status", "S") catch unreachable; m.put("cd_education_status", "E1") catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cd_marital_status", "M") catch unreachable; m.put("cd_education_status", "E2") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cd_marital_status", "W") catch unreachable; m.put("cd_education_status", "E3") catch unreachable; break :blk6 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_country", "United States") catch unreachable; m.put("ca_state", "TX") catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_country", "United States") catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(3))) catch unreachable; m.put("ca_country", "United States") catch unreachable; m.put("ca_state", "NY") catch unreachable; break :blk9 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk10 m; }};
    year = @as(i32,@intCast(2000));
    states1 = &[_][]const u8{"TX"};
    states2 = &[_][]const u8{"CA"};
    states3 = &[_][]const u8{"NY"};
    qty_base = blk11: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |ss| { for (customer_demographics) |cd| { if (!((ss.cdemo_sk == cd.cd_demo_sk))) continue; for (customer_address) |ca| { if (!((ss.addr_sk == ca.ca_address_sk))) continue; for (date_dim) |d| { if (!((ss.sold_date_sk == d.d_date_sk))) continue; if (!((((d.d_year == year) and (((((((std.mem.eql(u8, cd.cd_marital_status, "S") and std.mem.eql(u8, cd.cd_education_status, "E1")) and (ss.sales_price >= 100)) and (ss.sales_price <= 150))) or ((((std.mem.eql(u8, cd.cd_marital_status, "M") and std.mem.eql(u8, cd.cd_education_status, "E2")) and (ss.sales_price >= 50)) and (ss.sales_price <= 100)))) or ((((std.mem.eql(u8, cd.cd_marital_status, "W") and std.mem.eql(u8, cd.cd_education_status, "E3")) and (ss.sales_price >= 150)) and (ss.sales_price <= 200)))))) and ((((((_contains_list_string(states1, ca.ca_state) and (ss.net_profit >= @as(i32,@intCast(0)))) and (ss.net_profit <= @as(i32,@intCast(2000))))) or (((_contains_list_string(states2, ca.ca_state) and (ss.net_profit >= @as(i32,@intCast(150)))) and (ss.net_profit <= @as(i32,@intCast(3000)))))) or (((_contains_list_string(states3, ca.ca_state) and (ss.net_profit >= @as(i32,@intCast(50)))) and (ss.net_profit <= @as(i32,@intCast(25000)))))))))) continue; _tmp0.append(ss.quantity) catch unreachable; } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; };
    qty = qty_base;
    result = _sum_int(qty);
    _json(result);
    test_TPCDS_Q48_simplified();
}
