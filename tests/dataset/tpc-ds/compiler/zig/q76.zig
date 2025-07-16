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
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_part: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_part: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_part: []const std.AutoHashMap([]const u8, i32) = undefined;
var all_rows: []const i32 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q76_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "store") catch unreachable; m.put("col_name", 0) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "CatA") catch unreachable; m.put("sales_cnt", @as(i32,@intCast(1))) catch unreachable; m.put("sales_amt", 10) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "web") catch unreachable; m.put("col_name", 0) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "CatB") catch unreachable; m.put("sales_cnt", @as(i32,@intCast(1))) catch unreachable; m.put("sales_amt", 15) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "catalog") catch unreachable; m.put("col_name", 0) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "CatC") catch unreachable; m.put("sales_cnt", @as(i32,@intCast(1))) catch unreachable; m.put("sales_amt", 20) catch unreachable; break :blk2 m; }}));
}

pub fn main() void {
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "CatA") catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_category", "CatB") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(3))) catch unreachable; m.put("i_category", "CatC") catch unreachable; break :blk6 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", 0) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 10) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk7 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", 0) catch unreachable; m.put("ws_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ext_sales_price", 15) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk8 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", 0) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cs_ext_sales_price", 20) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }};
    store_part = blk11: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (item) |i| { if (!((i.i_item_sk == ss.ss_item_sk))) continue; for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; if (!((ss.ss_customer_sk == 0))) continue; _tmp0.append(blk10: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "store") catch unreachable; m.put("col_name", ss.ss_customer_sk) catch unreachable; m.put("d_year", d.d_year) catch unreachable; m.put("d_qoy", d.d_qoy) catch unreachable; m.put("i_category", i.i_category) catch unreachable; m.put("ext_sales_price", ss.ss_ext_sales_price) catch unreachable; break :blk10 m; }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; };
    web_part = blk13: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (item) |i| { if (!((i.i_item_sk == ws.ws_item_sk))) continue; for (date_dim) |d| { if (!((d.d_date_sk == ws.ws_sold_date_sk))) continue; if (!((ws.ws_bill_customer_sk == 0))) continue; _tmp2.append(blk12: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "web") catch unreachable; m.put("col_name", ws.ws_bill_customer_sk) catch unreachable; m.put("d_year", d.d_year) catch unreachable; m.put("d_qoy", d.d_qoy) catch unreachable; m.put("i_category", i.i_category) catch unreachable; m.put("ext_sales_price", ws.ws_ext_sales_price) catch unreachable; break :blk12 m; }) catch unreachable; } } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk13 _tmp3; };
    catalog_part = blk15: { var _tmp4 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (item) |i| { if (!((i.i_item_sk == cs.cs_item_sk))) continue; for (date_dim) |d| { if (!((d.d_date_sk == cs.cs_sold_date_sk))) continue; if (!((cs.cs_bill_customer_sk == 0))) continue; _tmp4.append(blk14: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "catalog") catch unreachable; m.put("col_name", cs.cs_bill_customer_sk) catch unreachable; m.put("d_year", d.d_year) catch unreachable; m.put("d_qoy", d.d_qoy) catch unreachable; m.put("i_category", i.i_category) catch unreachable; m.put("ext_sales_price", cs.cs_ext_sales_price) catch unreachable; break :blk14 m; }) catch unreachable; } } } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk15 _tmp5; };
    all_rows = concat(store_part, web_part, catalog_part);
    result = blk19: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(i32) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (all_rows) |r| { const _tmp10 = blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("channel", r.channel) catch unreachable; m.put("col_name", r.col_name) catch unreachable; m.put("d_year", r.d_year) catch unreachable; m.put("d_qoy", r.d_qoy) catch unreachable; m.put("i_category", r.i_category) catch unreachable; break :blk16 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(r) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(i32) }{ .key = _tmp10, .Items = std.ArrayList(i32).init(std.heap.page_allocator) }; g.Items.append(r) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("channel", g.key.channel) catch unreachable; m.put("col_name", g.key.col_name) catch unreachable; m.put("d_year", g.key.d_year) catch unreachable; m.put("d_qoy", g.key.d_qoy) catch unreachable; m.put("i_category", g.key.i_category) catch unreachable; m.put("sales_cnt", (g.Items.len)) catch unreachable; m.put("sales_amt", _sum_int(blk18: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.r.ext_sales_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk18 _tmp7; })) catch unreachable; break :blk17 m; }) catch unreachable; } break :blk19 _tmp11.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q76_simplified();
}
