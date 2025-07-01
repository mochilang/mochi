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

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var agg: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q79_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("c_last_name", "Smith") catch unreachable; m.put("c_first_name", "Alice") catch unreachable; m.put("s_city", "CityA") catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("amt", 5) catch unreachable; m.put("profit", 10) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_dow", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk1 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_city", "CityA") catch unreachable; m.put("s_number_employees", @as(i32,@intCast(250))) catch unreachable; break :blk2 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_dep_count", @as(i32,@intCast(2))) catch unreachable; m.put("hd_vehicle_count", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_coupon_amt", 5) catch unreachable; m.put("ss_net_profit", 10) catch unreachable; break :blk4 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_last_name", "Smith") catch unreachable; m.put("c_first_name", "Alice") catch unreachable; break :blk5 m; }};
    agg = blk10: { var _tmp4 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; for (store) |s| { if (!((s.s_store_sk == ss.ss_store_sk))) continue; for (household_demographics) |hd| { if (!((hd.hd_demo_sk == ss.ss_hdemo_sk))) continue; if (!((((((((hd.hd_dep_count == @as(i32,@intCast(2))) or (hd.hd_vehicle_count > @as(i32,@intCast(1))))) and (d.d_dow == @as(i32,@intCast(1)))) and ((((d.d_year == @as(i32,@intCast(1998))) or (d.d_year == @as(i32,@intCast(1999)))) or (d.d_year == @as(i32,@intCast(2000)))))) and (s.s_number_employees >= @as(i32,@intCast(200)))) and (s.s_number_employees <= @as(i32,@intCast(295)))))) continue; const _tmp6 = blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ticket", ss.ss_ticket_number) catch unreachable; m.put("customer_sk", ss.ss_customer_sk) catch unreachable; m.put("city", s.s_city) catch unreachable; break :blk6 m; }; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } } } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("key", g.key) catch unreachable; m.put("amt", _sum_int(blk8: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss.ss_coupon_amt) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk8 _tmp1; })) catch unreachable; m.put("profit", _sum_int(blk9: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.ss.ss_net_profit) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk9 _tmp3; })) catch unreachable; break :blk7 m; }) catch unreachable; } break :blk10 _tmp7.toOwnedSlice() catch unreachable; };
    result = blk12: { var _tmp8 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (agg) |a| { for (customer) |c| { if (!((c.c_customer_sk == a.key.customer_sk))) continue; _tmp8.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_last_name", c.c_last_name) catch unreachable; m.put("c_first_name", c.c_first_name) catch unreachable; m.put("s_city", a.key.city) catch unreachable; m.put("ss_ticket_number", a.key.ticket) catch unreachable; m.put("amt", a.amt) catch unreachable; m.put("profit", a.profit) catch unreachable; break :blk11 m; }) catch unreachable; } } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk12 _tmp9; };
    _json(result);
    test_TPCDS_Q79_simplified();
}
