const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _avg_float(v: []const f64) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += it; }
    return sum / @as(f64, @floatFromInt(v.len));
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

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const StoreSale = struct {
    ss_item_sk: i32,
    ss_list_price: f64,
    ss_quantity: i32,
    ss_sold_date_sk: i32,
};

const CatalogSale = struct {
    cs_item_sk: i32,
    cs_list_price: f64,
    cs_quantity: i32,
    cs_sold_date_sk: i32,
};

const WebSale = struct {
    ws_item_sk: i32,
    ws_list_price: f64,
    ws_quantity: i32,
    ws_sold_date_sk: i32,
};

const Item = struct {
    i_item_sk: i32,
    i_brand_id: i32,
    i_class_id: i32,
    i_category_id: i32,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
    d_moy: i32,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var cross_items: []const std.AutoHashMap([]const u8, i32) = undefined;
var avg_sales: f64 = undefined;
var store_filtered: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q14_cross_channel() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "store") catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_class_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(1))) catch unreachable; m.put("sales", 60) catch unreachable; m.put("number_sales", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_list_price", 10) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_list_price", 20) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(3))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; break :blk2 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_list_price", 10) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(2))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_list_price", 30) catch unreachable; m.put("ws_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_class_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(1))) catch unreachable; break :blk5 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("d_moy", @as(i32,@intCast(12))) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2002))) catch unreachable; m.put("d_moy", @as(i32,@intCast(11))) catch unreachable; break :blk7 m; }};
    cross_items = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk8 m; }};
    avg_sales = _avg_float(&[_]f64{20, 20, 30});
    store_filtered = blk14: { var _tmp6 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp7 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((((ss.ss_sold_date_sk == d.d_date_sk) and (d.d_year == @as(i32,@intCast(2002)))) and (d.d_moy == @as(i32,@intCast(11)))))) continue; if (!(_contains_list_int((blk13: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (cross_items) |ci| { _tmp4.append(ci.ss_item_sk) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk13 _tmp5; }), ss.ss_item_sk))) continue; const _tmp8 = blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("class_id", @as(i32,@intCast(1))) catch unreachable; m.put("category_id", @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }; if (_tmp7.get(_tmp8)) |idx| { _tmp6.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp8, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp6.append(g) catch unreachable; _tmp7.put(_tmp8, _tmp6.items.len - 1) catch unreachable; } } } var _tmp9 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp6.items) |g| { _tmp9.append(blk10: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "store") catch unreachable; m.put("sales", _sum_int(blk11: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append((x.ss_quantity * x.ss_list_price)) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; })) catch unreachable; m.put("number_sales", (blk12: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp2.append(_) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk12 _tmp3; }).len) catch unreachable; break :blk10 m; }) catch unreachable; } break :blk14 _tmp9.toOwnedSlice() catch unreachable; };
    result = blk16: { var _tmp10 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_filtered) |r| { if (!((r.sales > avg_sales))) continue; _tmp10.append(blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("channel", r.channel) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_class_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(1))) catch unreachable; m.put("sales", r.sales) catch unreachable; m.put("number_sales", r.number_sales) catch unreachable; break :blk15 m; }) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk16 _tmp11; };
    _json(result);
    test_TPCDS_Q14_cross_channel();
}
