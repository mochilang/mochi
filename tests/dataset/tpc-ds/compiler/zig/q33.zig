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

var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var month: i32 = undefined;
var year: i32 = undefined;
var union_sales: []const i32 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q33_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_manufact_id", @as(i32,@intCast(1))) catch unreachable; m.put("total_sales", 150) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_manufact_id", @as(i32,@intCast(2))) catch unreachable; m.put("total_sales", 50) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    item = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "Books") catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(2))) catch unreachable; m.put("i_category", "Books") catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("d_moy", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_gmt_offset", -@as(i32,@intCast(5))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_gmt_offset", -@as(i32,@intCast(5))) catch unreachable; break :blk6 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_ext_sales_price", 50) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_addr_sk", @as(i32,@intCast(2))) catch unreachable; break :blk8 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ext_sales_price", 20) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_sales_price", 30) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_bill_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk10 m; }};
    month = @as(i32,@intCast(1));
    year = @as(i32,@intCast(2000));
    union_sales = concat(blk12: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; for (customer_address) |ca| { if (!((ss.ss_addr_sk == ca.ca_address_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; if (!((((std.mem.eql(u8, i.i_category, "Books") and (d.d_year == year)) and (d.d_moy == month)) and (ca.ca_gmt_offset == (-@as(i32,@intCast(5))))))) continue; _tmp0.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("manu", i.i_manufact_id) catch unreachable; m.put("price", ss.ss_ext_sales_price) catch unreachable; break :blk11 m; }) catch unreachable; } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk12 _tmp1; }, blk14: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (date_dim) |d| { if (!((cs.cs_sold_date_sk == d.d_date_sk))) continue; for (customer_address) |ca| { if (!((cs.cs_bill_addr_sk == ca.ca_address_sk))) continue; for (item) |i| { if (!((cs.cs_item_sk == i.i_item_sk))) continue; if (!((((std.mem.eql(u8, i.i_category, "Books") and (d.d_year == year)) and (d.d_moy == month)) and (ca.ca_gmt_offset == (-@as(i32,@intCast(5))))))) continue; _tmp2.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("manu", i.i_manufact_id) catch unreachable; m.put("price", cs.cs_ext_sales_price) catch unreachable; break :blk13 m; }) catch unreachable; } } } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk14 _tmp3; }, blk16: { var _tmp4 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (date_dim) |d| { if (!((ws.ws_sold_date_sk == d.d_date_sk))) continue; for (customer_address) |ca| { if (!((ws.ws_bill_addr_sk == ca.ca_address_sk))) continue; for (item) |i| { if (!((ws.ws_item_sk == i.i_item_sk))) continue; if (!((((std.mem.eql(u8, i.i_category, "Books") and (d.d_year == year)) and (d.d_moy == month)) and (ca.ca_gmt_offset == (-@as(i32,@intCast(5))))))) continue; _tmp4.append(blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("manu", i.i_manufact_id) catch unreachable; m.put("price", ws.ws_ext_sales_price) catch unreachable; break :blk15 m; }) catch unreachable; } } } } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk16 _tmp5; });
    result = blk19: { var _tmp8 = std.ArrayList(struct { key: i32, Items: std.ArrayList(i32) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (union_sales) |s| { const _tmp10 = s.manu; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(s) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(i32) }{ .key = _tmp10, .Items = std.ArrayList(i32).init(std.heap.page_allocator) }; g.Items.append(s) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_manufact_id", g.key) catch unreachable; m.put("total_sales", _sum_int(blk18: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk18 _tmp7; })) catch unreachable; break :blk17 m; }) catch unreachable; } break :blk19 _tmp11.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q33_simplified();
}
