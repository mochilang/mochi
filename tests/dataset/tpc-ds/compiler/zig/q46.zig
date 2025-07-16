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

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var depcnt: i32 = undefined;
var vehcnt: i32 = undefined;
var year: i32 = undefined;
var cities: []const []const u8 = undefined;
var dn: []const std.AutoHashMap([]const u8, i32) = undefined;
var base: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q46_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("c_last_name", "Doe") catch unreachable; m.put("c_first_name", "John") catch unreachable; m.put("ca_city", "Seattle") catch unreachable; m.put("bought_city", "Portland") catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("amt", 5) catch unreachable; m.put("profit", 20) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_coupon_amt", 5) catch unreachable; m.put("ss_net_profit", 20) catch unreachable; break :blk1 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_dow", @as(i32,@intCast(6))) catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk2 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_city", "CityA") catch unreachable; break :blk3 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_dep_count", @as(i32,@intCast(2))) catch unreachable; m.put("hd_vehicle_count", @as(i32,@intCast(0))) catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_city", "Portland") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_city", "Seattle") catch unreachable; break :blk6 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_last_name", "Doe") catch unreachable; m.put("c_first_name", "John") catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(2))) catch unreachable; break :blk7 m; }};
    depcnt = @as(i32,@intCast(2));
    vehcnt = @as(i32,@intCast(0));
    year = @as(i32,@intCast(2020));
    cities = &[_][]const u8{"CityA"};
    dn = blk12: { var _tmp4 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; for (household_demographics) |hd| { if (!((ss.ss_hdemo_sk == hd.hd_demo_sk))) continue; for (customer_address) |ca| { if (!((ss.ss_addr_sk == ca.ca_address_sk))) continue; if (!(((((((hd.hd_dep_count == depcnt) or (hd.hd_vehicle_count == vehcnt))) and _contains_list_int(&[_]i32{@as(i32,@intCast(6)), @as(i32,@intCast(0))}, d.d_dow)) and (d.d_year == year)) and _contains_list_string(cities, s.s_city)))) continue; const _tmp6 = blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_ticket_number", ss.ss_ticket_number) catch unreachable; m.put("ss_customer_sk", ss.ss_customer_sk) catch unreachable; m.put("ca_city", ca.ca_city) catch unreachable; break :blk8 m; }; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } } } } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_ticket_number", g.key.ss_ticket_number) catch unreachable; m.put("ss_customer_sk", g.key.ss_customer_sk) catch unreachable; m.put("bought_city", g.key.ca_city) catch unreachable; m.put("amt", _sum_int(blk10: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss.ss_coupon_amt) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk10 _tmp1; })) catch unreachable; m.put("profit", _sum_int(blk11: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.ss.ss_net_profit) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk11 _tmp3; })) catch unreachable; break :blk9 m; }) catch unreachable; } break :blk12 _tmp7.toOwnedSlice() catch unreachable; };
    base = blk14: { var _tmp8 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (dn) |dnrec| { for (customer) |c| { if (!((dnrec.ss_customer_sk == c.c_customer_sk))) continue; for (customer_address) |current_addr| { if (!((c.c_current_addr_sk == current_addr.ca_address_sk))) continue; if (!((current_addr.ca_city != dnrec.bought_city))) continue; _tmp8.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_last_name", c.c_last_name) catch unreachable; m.put("c_first_name", c.c_first_name) catch unreachable; m.put("ca_city", current_addr.ca_city) catch unreachable; m.put("bought_city", dnrec.bought_city) catch unreachable; m.put("ss_ticket_number", dnrec.ss_ticket_number) catch unreachable; m.put("amt", dnrec.amt) catch unreachable; m.put("profit", dnrec.profit) catch unreachable; break :blk13 m; }) catch unreachable; } } } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk14 _tmp9; };
    result = base;
    _json(result);
    test_TPCDS_Q46_simplified();
}
