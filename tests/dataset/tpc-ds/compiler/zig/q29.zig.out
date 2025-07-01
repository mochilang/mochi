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
    ss_sold_date_sk: i32,
    ss_item_sk: i32,
    ss_store_sk: i32,
    ss_customer_sk: i32,
    ss_quantity: i32,
    ss_ticket_number: i32,
};

const StoreReturn = struct {
    sr_returned_date_sk: i32,
    sr_item_sk: i32,
    sr_customer_sk: i32,
    sr_ticket_number: i32,
    sr_return_quantity: i32,
};

const CatalogSale = struct {
    cs_sold_date_sk: i32,
    cs_item_sk: i32,
    cs_bill_customer_sk: i32,
    cs_quantity: i32,
};

const DateDim = struct {
    d_date_sk: i32,
    d_moy: i32,
    d_year: i32,
};

const Store = struct {
    s_store_sk: i32,
    s_store_id: []const u8,
    s_store_name: []const u8,
};

const Item = struct {
    i_item_sk: i32,
    i_item_id: []const u8,
    i_item_desc: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var base: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q29_quantity_summary() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "ITEM1") catch unreachable; m.put("i_item_desc", "Desc1") catch unreachable; m.put("s_store_id", "S1") catch unreachable; m.put("s_store_name", "Store1") catch unreachable; m.put("store_sales_quantity", @as(i32,@intCast(10))) catch unreachable; m.put("store_returns_quantity", @as(i32,@intCast(2))) catch unreachable; m.put("catalog_sales_quantity", @as(i32,@intCast(5))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(10))) catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_returned_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("sr_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("sr_return_quantity", @as(i32,@intCast(2))) catch unreachable; break :blk2 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(5))) catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_moy", @as(i32,@intCast(4))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_moy", @as(i32,@intCast(5))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_moy", @as(i32,@intCast(5))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk6 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_store_id", "S1") catch unreachable; m.put("s_store_name", "Store1") catch unreachable; break :blk7 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "ITEM1") catch unreachable; m.put("i_item_desc", "Desc1") catch unreachable; break :blk8 m; }};
    base = blk10: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (store_returns) |sr| { if (!(((ss.ss_ticket_number == sr.sr_ticket_number) and (ss.ss_item_sk == sr.sr_item_sk)))) continue; for (catalog_sales) |cs| { if (!(((sr.sr_customer_sk == cs.cs_bill_customer_sk) and (sr.sr_item_sk == cs.cs_item_sk)))) continue; for (date_dim) |d1| { if (!((d1.d_date_sk == ss.ss_sold_date_sk))) continue; for (date_dim) |d2| { if (!((d2.d_date_sk == sr.sr_returned_date_sk))) continue; for (date_dim) |d3| { if (!((d3.d_date_sk == cs.cs_sold_date_sk))) continue; for (store) |s| { if (!((s.s_store_sk == ss.ss_store_sk))) continue; for (item) |i| { if (!((i.i_item_sk == ss.ss_item_sk))) continue; if (!((((((d1.d_moy == @as(i32,@intCast(4))) and (d1.d_year == @as(i32,@intCast(1999)))) and (d2.d_moy >= @as(i32,@intCast(4)))) and (d2.d_moy <= @as(i32,@intCast(7)))) and _contains_list_int(&[_]i32{@as(i32,@intCast(1999)), @as(i32,@intCast(2000)), @as(i32,@intCast(2001))}, d3.d_year)))) continue; _tmp0.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", ss.ss_quantity) catch unreachable; m.put("sr_return_quantity", sr.sr_return_quantity) catch unreachable; m.put("cs_quantity", cs.cs_quantity) catch unreachable; m.put("i_item_id", i.i_item_id) catch unreachable; m.put("i_item_desc", i.i_item_desc) catch unreachable; m.put("s_store_id", s.s_store_id) catch unreachable; m.put("s_store_name", s.s_store_name) catch unreachable; break :blk9 m; }) catch unreachable; } } } } } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk10 _tmp1; };
    result = blk16: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (base) |b| { const _tmp10 = blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item_id", b.i_item_id) catch unreachable; m.put("item_desc", b.i_item_desc) catch unreachable; m.put("s_store_id", b.s_store_id) catch unreachable; m.put("s_store_name", b.s_store_name) catch unreachable; break :blk11 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(b) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(b) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.item_id) catch unreachable; m.put("i_item_desc", g.key.item_desc) catch unreachable; m.put("s_store_id", g.key.s_store_id) catch unreachable; m.put("s_store_name", g.key.s_store_name) catch unreachable; m.put("store_sales_quantity", _sum_int(blk13: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.ss_quantity) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk13 _tmp3; })) catch unreachable; m.put("store_returns_quantity", _sum_int(blk14: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.sr_return_quantity) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk14 _tmp5; })) catch unreachable; m.put("catalog_sales_quantity", _sum_int(blk15: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.cs_quantity) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk15 _tmp7; })) catch unreachable; break :blk12 m; }) catch unreachable; } break :blk16 _tmp11.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q29_quantity_summary();
}
