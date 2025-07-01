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

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const StoreSale = struct {
    ss_item_sk: i32,
    ss_sold_date_sk: i32,
    ss_customer_sk: i32,
    ss_quantity: i32,
    ss_sales_price: f64,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
    d_moy: i32,
};

const Item = struct {
    i_item_sk: i32,
};

const CatalogSale = struct {
    cs_sold_date_sk: i32,
    cs_item_sk: i32,
    cs_bill_customer_sk: i32,
    cs_quantity: i32,
    cs_list_price: f64,
};

const WebSale = struct {
    ws_sold_date_sk: i32,
    ws_item_sk: i32,
    ws_bill_customer_sk: i32,
    ws_quantity: i32,
    ws_list_price: f64,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var frequent_ss_items: []const i32 = undefined;
var customer_totals: []const std.AutoHashMap([]const u8, i32) = undefined;
var max_sales: i32 = undefined;
var best_ss_customer: []const i32 = undefined;
var catalog: []const i32 = undefined;
var web: []const i32 = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q23_cross_channel_sales() void {
    expect((result == 50));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk7 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("d_moy", @as(i32,@intCast(1))) catch unreachable; break :blk8 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; break :blk10 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(2))) catch unreachable; m.put("cs_list_price", 10) catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_bill_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(2))) catch unreachable; m.put("cs_list_price", 10) catch unreachable; break :blk12 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_quantity", @as(i32,@intCast(3))) catch unreachable; m.put("ws_list_price", 10) catch unreachable; break :blk13 m; }, blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_bill_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("ws_list_price", 10) catch unreachable; break :blk14 m; }};
    frequent_ss_items = blk16: { var _tmp0 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp1 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; if (!((d.d_year == @as(i32,@intCast(2000))))) continue; const _tmp2 = blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item_sk", i.i_item_sk) catch unreachable; m.put("date_sk", d.d_date_sk) catch unreachable; break :blk15 m; }; if (_tmp1.get(_tmp2)) |idx| { _tmp0.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp0.append(g) catch unreachable; _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable; } } } } var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp0.items) |g| { _tmp3.append(g.key.item_sk) catch unreachable; } break :blk16 _tmp3.toOwnedSlice() catch unreachable; };
    customer_totals = blk19: { var _tmp6 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp7 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (store_sales) |ss| { const _tmp8 = ss.ss_customer_sk; if (_tmp7.get(_tmp8)) |idx| { _tmp6.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp8, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp6.append(g) catch unreachable; _tmp7.put(_tmp8, _tmp6.items.len - 1) catch unreachable; } } var _tmp9 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp6.items) |g| { _tmp9.append(blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", g.key) catch unreachable; m.put("sales", _sum_int(blk18: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append((x.ss_quantity * x.ss_sales_price)) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk18 _tmp5; })) catch unreachable; break :blk17 m; }) catch unreachable; } break :blk19 _tmp9.toOwnedSlice() catch unreachable; };
    max_sales = max(blk20: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (customer_totals) |c| { _tmp10.append(c.sales) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk20 _tmp11; });
    best_ss_customer = blk21: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (customer_totals) |c| { if (!((c.sales > (0.95 * max_sales)))) continue; _tmp12.append(c.cust) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk21 _tmp13; };
    catalog = blk22: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (date_dim) |d| { if (!((cs.cs_sold_date_sk == d.d_date_sk))) continue; if (!(((((d.d_year == @as(i32,@intCast(2000))) and (d.d_moy == @as(i32,@intCast(1)))) and _contains_list_int(best_ss_customer, cs.cs_bill_customer_sk)) and _contains_list_int(frequent_ss_items, cs.cs_item_sk)))) continue; _tmp14.append((cs.cs_quantity * cs.cs_list_price)) catch unreachable; } } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk22 _tmp15; };
    web = blk23: { var _tmp16 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |ws| { for (date_dim) |d| { if (!((ws.ws_sold_date_sk == d.d_date_sk))) continue; if (!(((((d.d_year == @as(i32,@intCast(2000))) and (d.d_moy == @as(i32,@intCast(1)))) and _contains_list_int(best_ss_customer, ws.ws_bill_customer_sk)) and _contains_list_int(frequent_ss_items, ws.ws_item_sk)))) continue; _tmp16.append((ws.ws_quantity * ws.ws_list_price)) catch unreachable; } } const _tmp17 = _tmp16.toOwnedSlice() catch unreachable; break :blk23 _tmp17; };
    result = (_sum_int(catalog) + _sum_int(web));
    _json(result);
    test_TPCDS_Q23_cross_channel_sales();
}
