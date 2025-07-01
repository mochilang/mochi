const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains(comptime T: type, v: []const T, item: T) bool {
    for (v) |it| { if (std.meta.eql(it, item)) return true; }
    return false;
}

fn _union_all(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _union(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { if (!_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _except(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (!_contains(T, b, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _intersect(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
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

var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var year_total: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q4_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("customer_id", "C1") catch unreachable; m.put("customer_first_name", "Alice") catch unreachable; m.put("customer_last_name", "A") catch unreachable; m.put("customer_login", "alice") catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    customer = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_customer_id", "C1") catch unreachable; m.put("c_first_name", "Alice") catch unreachable; m.put("c_last_name", "A") catch unreachable; m.put("c_login", "alice") catch unreachable; break :blk1 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_list_price", 10) catch unreachable; m.put("ss_ext_wholesale_cost", 5) catch unreachable; m.put("ss_ext_discount_amt", 0) catch unreachable; m.put("ss_ext_sales_price", 10) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_ext_list_price", 20) catch unreachable; m.put("ss_ext_wholesale_cost", 5) catch unreachable; m.put("ss_ext_discount_amt", 0) catch unreachable; m.put("ss_ext_sales_price", 20) catch unreachable; break :blk3 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ext_list_price", 10) catch unreachable; m.put("cs_ext_wholesale_cost", 2) catch unreachable; m.put("cs_ext_discount_amt", 0) catch unreachable; m.put("cs_ext_sales_price", 10) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_ext_list_price", 30) catch unreachable; m.put("cs_ext_wholesale_cost", 2) catch unreachable; m.put("cs_ext_discount_amt", 0) catch unreachable; m.put("cs_ext_sales_price", 30) catch unreachable; break :blk5 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_list_price", 10) catch unreachable; m.put("ws_ext_wholesale_cost", 5) catch unreachable; m.put("ws_ext_discount_amt", 0) catch unreachable; m.put("ws_ext_sales_price", 10) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ext_list_price", 12) catch unreachable; m.put("ws_ext_wholesale_cost", 5) catch unreachable; m.put("ws_ext_discount_amt", 0) catch unreachable; m.put("ws_ext_sales_price", 12) catch unreachable; break :blk7 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2001))) catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2002))) catch unreachable; break :blk9 m; }};
    year_total = _union_all(i32, _union_all(i32, (blk13: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (store_sales) |s| { if (!((c.c_customer_sk == s.ss_customer_sk))) continue; for (date_dim) |d| { if (!((s.ss_sold_date_sk == d.d_date_sk))) continue; const _tmp4 = blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", c.c_customer_id) catch unreachable; m.put(first, c.c_first_name) catch unreachable; m.put("last", c.c_last_name) catch unreachable; m.put("login", c.c_login) catch unreachable; m.put("year", d.d_year) catch unreachable; break :blk10 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", g.key.id) catch unreachable; m.put("customer_first_name", g.key.first) catch unreachable; m.put("customer_last_name", g.key.last) catch unreachable; m.put("customer_login", g.key.login) catch unreachable; m.put("dyear", g.key.year) catch unreachable; m.put(year_total, _sum_int(blk12: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(((((((x.ss_ext_list_price - x.ss_ext_wholesale_cost) - x.ss_ext_discount_amt)) + x.ss_ext_sales_price)) / @as(i32,@intCast(2)))) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk12 _tmp1; })) catch unreachable; m.put("sale_type", "s") catch unreachable; break :blk11 m; }) catch unreachable; } break :blk13 _tmp5.toOwnedSlice() catch unreachable; }), (blk17: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (catalog_sales) |cs| { if (!((c.c_customer_sk == cs.cs_bill_customer_sk))) continue; for (date_dim) |d| { if (!((cs.cs_sold_date_sk == d.d_date_sk))) continue; const _tmp10 = blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", c.c_customer_id) catch unreachable; m.put(first, c.c_first_name) catch unreachable; m.put("last", c.c_last_name) catch unreachable; m.put("login", c.c_login) catch unreachable; m.put("year", d.d_year) catch unreachable; break :blk14 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", g.key.id) catch unreachable; m.put("customer_first_name", g.key.first) catch unreachable; m.put("customer_last_name", g.key.last) catch unreachable; m.put("customer_login", g.key.login) catch unreachable; m.put("dyear", g.key.year) catch unreachable; m.put(year_total, _sum_int(blk16: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(((((((x.cs_ext_list_price - x.cs_ext_wholesale_cost) - x.cs_ext_discount_amt)) + x.cs_ext_sales_price)) / @as(i32,@intCast(2)))) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk16 _tmp7; })) catch unreachable; m.put("sale_type", "c") catch unreachable; break :blk15 m; }) catch unreachable; } break :blk17 _tmp11.toOwnedSlice() catch unreachable; })), (blk21: { var _tmp14 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp15 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (web_sales) |ws| { if (!((c.c_customer_sk == ws.ws_bill_customer_sk))) continue; for (date_dim) |d| { if (!((ws.ws_sold_date_sk == d.d_date_sk))) continue; const _tmp16 = blk18: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", c.c_customer_id) catch unreachable; m.put(first, c.c_first_name) catch unreachable; m.put("last", c.c_last_name) catch unreachable; m.put("login", c.c_login) catch unreachable; m.put("year", d.d_year) catch unreachable; break :blk18 m; }; if (_tmp15.get(_tmp16)) |idx| { _tmp14.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp16, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp14.append(g) catch unreachable; _tmp15.put(_tmp16, _tmp14.items.len - 1) catch unreachable; } } } } var _tmp17 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp14.items) |g| { _tmp17.append(blk19: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", g.key.id) catch unreachable; m.put("customer_first_name", g.key.first) catch unreachable; m.put("customer_last_name", g.key.last) catch unreachable; m.put("customer_login", g.key.login) catch unreachable; m.put("dyear", g.key.year) catch unreachable; m.put(year_total, _sum_int(blk20: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp12.append(((((((x.ws_ext_list_price - x.ws_ext_wholesale_cost) - x.ws_ext_discount_amt)) + x.ws_ext_sales_price)) / @as(i32,@intCast(2)))) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk20 _tmp13; })) catch unreachable; m.put("sale_type", "w") catch unreachable; break :blk19 m; }) catch unreachable; } break :blk21 _tmp17.toOwnedSlice() catch unreachable; }));
    result = blk23: { var _tmp18 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (year_total) |s1| { for (year_total) |s2| { if (!((s2.customer_id == s1.customer_id))) continue; for (year_total) |c1| { if (!((c1.customer_id == s1.customer_id))) continue; for (year_total) |c2| { if (!((c2.customer_id == s1.customer_id))) continue; for (year_total) |w1| { if (!((w1.customer_id == s1.customer_id))) continue; for (year_total) |w2| { if (!((w2.customer_id == s1.customer_id))) continue; if (!(((((((((((((((((std.mem.eql(u8, s1.sale_type, "s") and std.mem.eql(u8, c1.sale_type, "c")) and std.mem.eql(u8, w1.sale_type, "w")) and std.mem.eql(u8, s2.sale_type, "s")) and std.mem.eql(u8, c2.sale_type, "c")) and std.mem.eql(u8, w2.sale_type, "w")) and (s1.dyear == @as(i32,@intCast(2001)))) and (s2.dyear == @as(i32,@intCast(2002)))) and (c1.dyear == @as(i32,@intCast(2001)))) and (c2.dyear == @as(i32,@intCast(2002)))) and (w1.dyear == @as(i32,@intCast(2001)))) and (w2.dyear == @as(i32,@intCast(2002)))) and (s1.year_total > @as(i32,@intCast(0)))) and (c1.year_total > @as(i32,@intCast(0)))) and (w1.year_total > @as(i32,@intCast(0)))) and ((if ((c1.year_total > @as(i32,@intCast(0)))) ((c2.year_total / c1.year_total)) else (0)) > (if ((s1.year_total > @as(i32,@intCast(0)))) ((s2.year_total / s1.year_total)) else (0)))) and ((if ((c1.year_total > @as(i32,@intCast(0)))) ((c2.year_total / c1.year_total)) else (0)) > (if ((w1.year_total > @as(i32,@intCast(0)))) ((w2.year_total / w1.year_total)) else (0)))))) continue; _tmp18.append(blk22: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", s2.customer_id) catch unreachable; m.put("customer_first_name", s2.customer_first_name) catch unreachable; m.put("customer_last_name", s2.customer_last_name) catch unreachable; m.put("customer_login", s2.customer_login) catch unreachable; break :blk22 m; }) catch unreachable; } } } } } } const _tmp19 = _tmp18.toOwnedSlice() catch unreachable; break :blk23 _tmp19; };
    _json(result);
    test_TPCDS_Q4_result();
}
