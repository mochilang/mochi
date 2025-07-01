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
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var sales_detail: []const i32 = undefined;
var all_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var prev_yr: i32 = undefined;
var curr_yr: i32 = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q75_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("prev_year", @as(i32,@intCast(2000))) catch unreachable; m.put("year", @as(i32,@intCast(2001))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_class_id", @as(i32,@intCast(2))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(3))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(4))) catch unreachable; m.put("prev_yr_cnt", @as(i32,@intCast(100))) catch unreachable; m.put("curr_yr_cnt", @as(i32,@intCast(80))) catch unreachable; m.put("sales_cnt_diff", -@as(i32,@intCast(20))) catch unreachable; m.put("sales_amt_diff", -200) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2001))) catch unreachable; break :blk2 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(50))) catch unreachable; m.put("ss_sales_price", 500) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(40))) catch unreachable; m.put("ss_sales_price", 400) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; break :blk4 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_quantity", @as(i32,@intCast(30))) catch unreachable; m.put("ws_sales_price", 300) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_quantity", @as(i32,@intCast(25))) catch unreachable; m.put("ws_sales_price", 250) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; break :blk6 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(20))) catch unreachable; m.put("cs_sales_price", 200) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(15))) catch unreachable; m.put("cs_sales_price", 150) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; break :blk8 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_class_id", @as(i32,@intCast(2))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(3))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(4))) catch unreachable; m.put("i_category", "Electronics") catch unreachable; break :blk9 m; }};
    sales_detail = concat(blk11: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; _tmp0.append(blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", d.d_year) catch unreachable; m.put("i_item_sk", ss.ss_item_sk) catch unreachable; m.put("quantity", ss.ss_quantity) catch unreachable; m.put("amount", ss.ss_sales_price) catch unreachable; break :blk10 m; }) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; }, blk13: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (date_dim) |d| { if (!((d.d_date_sk == ws.ws_sold_date_sk))) continue; _tmp2.append(blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", d.d_year) catch unreachable; m.put("i_item_sk", ws.ws_item_sk) catch unreachable; m.put("quantity", ws.ws_quantity) catch unreachable; m.put("amount", ws.ws_sales_price) catch unreachable; break :blk12 m; }) catch unreachable; } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk13 _tmp3; }, blk15: { var _tmp4 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (date_dim) |d| { if (!((d.d_date_sk == cs.cs_sold_date_sk))) continue; _tmp4.append(blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", d.d_year) catch unreachable; m.put("i_item_sk", cs.cs_item_sk) catch unreachable; m.put("quantity", cs.cs_quantity) catch unreachable; m.put("amount", cs.cs_sales_price) catch unreachable; break :blk14 m; }) catch unreachable; } } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk15 _tmp5; });
    all_sales = blk20: { var _tmp10 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(i32) }).init(std.heap.page_allocator); var _tmp11 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (sales_detail) |sd| { for (item) |i| { if (!((i.i_item_sk == sd.i_item_sk))) continue; if (!(std.mem.eql(u8, i.i_category, "Electronics"))) continue; const _tmp12 = blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("year", sd.d_year) catch unreachable; m.put("brand_id", i.i_brand_id) catch unreachable; m.put("class_id", i.i_class_id) catch unreachable; m.put("category_id", i.i_category_id) catch unreachable; m.put("manuf_id", i.i_manufact_id) catch unreachable; break :blk16 m; }; if (_tmp11.get(_tmp12)) |idx| { _tmp10.items[idx].Items.append(sd) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(i32) }{ .key = _tmp12, .Items = std.ArrayList(i32).init(std.heap.page_allocator) }; g.Items.append(sd) catch unreachable; _tmp10.append(g) catch unreachable; _tmp11.put(_tmp12, _tmp10.items.len - 1) catch unreachable; } } } var _tmp13 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp13.append(blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", g.key.year) catch unreachable; m.put("i_brand_id", g.key.brand_id) catch unreachable; m.put("i_class_id", g.key.class_id) catch unreachable; m.put("i_category_id", g.key.category_id) catch unreachable; m.put("i_manufact_id", g.key.manuf_id) catch unreachable; m.put("sales_cnt", _sum_int(blk18: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.sd.quantity) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk18 _tmp7; })) catch unreachable; m.put("sales_amt", _sum_int(blk19: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp8.append(x.sd.amount) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk19 _tmp9; })) catch unreachable; break :blk17 m; }) catch unreachable; } break :blk20 _tmp13.toOwnedSlice() catch unreachable; };
    prev_yr = first(blk21: { var _tmp14 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (all_sales) |a| { if (!((a.d_year == @as(i32,@intCast(2000))))) continue; _tmp14.append(a) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk21 _tmp15; });
    curr_yr = first(blk22: { var _tmp16 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (all_sales) |a| { if (!((a.d_year == @as(i32,@intCast(2001))))) continue; _tmp16.append(a) catch unreachable; } const _tmp17 = _tmp16.toOwnedSlice() catch unreachable; break :blk22 _tmp17; });
    result = if ((((curr_yr.sales_cnt / prev_yr.sales_cnt)) < 0.9)) (&[_]std.AutoHashMap([]const u8, i32){blk23: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("prev_year", prev_yr.d_year) catch unreachable; m.put("year", curr_yr.d_year) catch unreachable; m.put("i_brand_id", curr_yr.i_brand_id) catch unreachable; m.put("i_class_id", curr_yr.i_class_id) catch unreachable; m.put("i_category_id", curr_yr.i_category_id) catch unreachable; m.put("i_manufact_id", curr_yr.i_manufact_id) catch unreachable; m.put("prev_yr_cnt", prev_yr.sales_cnt) catch unreachable; m.put("curr_yr_cnt", curr_yr.sales_cnt) catch unreachable; m.put("sales_cnt_diff", (curr_yr.sales_cnt - prev_yr.sales_cnt)) catch unreachable; m.put("sales_amt_diff", (curr_yr.sales_amt - prev_yr.sales_amt)) catch unreachable; break :blk23 m; }}) else (&[_]i32{});
    _json(result);
    test_TPCDS_Q75_simplified();
}
