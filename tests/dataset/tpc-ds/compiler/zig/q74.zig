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

var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var year_total: []const i32 = undefined;
var s_firstyear: i32 = undefined;
var s_secyear: i32 = undefined;
var w_firstyear: i32 = undefined;
var w_secyear: i32 = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q74_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", @as(i32,@intCast(1))) catch unreachable; m.put("customer_first_name", "Alice") catch unreachable; m.put("customer_last_name", "Smith") catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    customer = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_customer_id", @as(i32,@intCast(1))) catch unreachable; m.put("c_first_name", "Alice") catch unreachable; m.put("c_last_name", "Smith") catch unreachable; break :blk1 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk3 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_paid", 100) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_net_paid", 110) catch unreachable; break :blk5 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_net_paid", 40) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_net_paid", 80) catch unreachable; break :blk7 m; }};
    year_total = concat(blk11: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (store_sales) |ss| { if (!((c.c_customer_sk == ss.ss_customer_sk))) continue; for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; if (!(((d.d_year == @as(i32,@intCast(1998))) or (d.d_year == @as(i32,@intCast(1999)))))) continue; const _tmp4 = blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", c.c_customer_id) catch unreachable; m.put(first, c.c_first_name) catch unreachable; m.put("last", c.c_last_name) catch unreachable; m.put("year", d.d_year) catch unreachable; break :blk8 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", g.key.id) catch unreachable; m.put("customer_first_name", g.key.first) catch unreachable; m.put("customer_last_name", g.key.last) catch unreachable; m.put("year", g.key.year) catch unreachable; m.put(year_total, _sum_int(blk10: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss.ss_net_paid) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk10 _tmp1; })) catch unreachable; m.put("sale_type", "s") catch unreachable; break :blk9 m; }) catch unreachable; } break :blk11 _tmp5.toOwnedSlice() catch unreachable; }, blk15: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (web_sales) |ws| { if (!((c.c_customer_sk == ws.ws_bill_customer_sk))) continue; for (date_dim) |d| { if (!((d.d_date_sk == ws.ws_sold_date_sk))) continue; if (!(((d.d_year == @as(i32,@intCast(1998))) or (d.d_year == @as(i32,@intCast(1999)))))) continue; const _tmp10 = blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", c.c_customer_id) catch unreachable; m.put(first, c.c_first_name) catch unreachable; m.put("last", c.c_last_name) catch unreachable; m.put("year", d.d_year) catch unreachable; break :blk12 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", g.key.id) catch unreachable; m.put("customer_first_name", g.key.first) catch unreachable; m.put("customer_last_name", g.key.last) catch unreachable; m.put("year", g.key.year) catch unreachable; m.put(year_total, _sum_int(blk14: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.ws.ws_net_paid) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk14 _tmp7; })) catch unreachable; m.put("sale_type", "w") catch unreachable; break :blk13 m; }) catch unreachable; } break :blk15 _tmp11.toOwnedSlice() catch unreachable; });
    s_firstyear = first(blk16: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (year_total) |y| { if (!((std.mem.eql(u8, y.sale_type, "s") and (y.year == @as(i32,@intCast(1998)))))) continue; _tmp12.append(y) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk16 _tmp13; });
    s_secyear = first(blk17: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (year_total) |y| { if (!((std.mem.eql(u8, y.sale_type, "s") and (y.year == @as(i32,@intCast(1999)))))) continue; _tmp14.append(y) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk17 _tmp15; });
    w_firstyear = first(blk18: { var _tmp16 = std.ArrayList(i32).init(std.heap.page_allocator); for (year_total) |y| { if (!((std.mem.eql(u8, y.sale_type, "w") and (y.year == @as(i32,@intCast(1998)))))) continue; _tmp16.append(y) catch unreachable; } const _tmp17 = _tmp16.toOwnedSlice() catch unreachable; break :blk18 _tmp17; });
    w_secyear = first(blk19: { var _tmp18 = std.ArrayList(i32).init(std.heap.page_allocator); for (year_total) |y| { if (!((std.mem.eql(u8, y.sale_type, "w") and (y.year == @as(i32,@intCast(1999)))))) continue; _tmp18.append(y) catch unreachable; } const _tmp19 = _tmp18.toOwnedSlice() catch unreachable; break :blk19 _tmp19; });
    result = if ((((s_firstyear.year_total > @as(i32,@intCast(0))) and (w_firstyear.year_total > @as(i32,@intCast(0)))) and (((w_secyear.year_total / w_firstyear.year_total)) > ((s_secyear.year_total / s_firstyear.year_total))))) (&[_]std.AutoHashMap([]const u8, i32){blk20: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_id", s_secyear.customer_id) catch unreachable; m.put("customer_first_name", s_secyear.customer_first_name) catch unreachable; m.put("customer_last_name", s_secyear.customer_last_name) catch unreachable; break :blk20 m; }}) else (&[_]i32{});
    _json(result);
    test_TPCDS_Q74_simplified();
}
