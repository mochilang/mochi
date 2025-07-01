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

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var month: i32 = undefined;
var year: i32 = undefined;
var records: []const std.AutoHashMap([]const u8, i32) = undefined;
var base: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q42_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(200))) catch unreachable; m.put("i_category", "CatB") catch unreachable; m.put("sum_ss_ext_sales_price", 20) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(100))) catch unreachable; m.put("i_category", "CatA") catch unreachable; m.put("sum_ss_ext_sales_price", 10) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ext_sales_price", 10) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ext_sales_price", 20) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ext_sales_price", 15) catch unreachable; break :blk4 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_manager_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(100))) catch unreachable; m.put("i_category", "CatA") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_manager_id", @as(i32,@intCast(2))) catch unreachable; m.put("i_category_id", @as(i32,@intCast(200))) catch unreachable; m.put("i_category", "CatB") catch unreachable; break :blk6 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; m.put("d_moy", @as(i32,@intCast(5))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_year", @as(i32,@intCast(2021))) catch unreachable; m.put("d_moy", @as(i32,@intCast(5))) catch unreachable; break :blk8 m; }};
    month = @as(i32,@intCast(5));
    year = @as(i32,@intCast(2020));
    records = blk10: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (date_dim) |dt| { for (store_sales) |ss| { if (!((ss.sold_date_sk == dt.d_date_sk))) continue; for (item) |it| { if (!((ss.item_sk == it.i_item_sk))) continue; if (!((((it.i_manager_id == @as(i32,@intCast(1))) and (dt.d_moy == month)) and (dt.d_year == year)))) continue; _tmp0.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", dt.d_year) catch unreachable; m.put("i_category_id", it.i_category_id) catch unreachable; m.put("i_category", it.i_category) catch unreachable; m.put("price", ss.ext_sales_price) catch unreachable; break :blk9 m; }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk10 _tmp1; };
    base = blk14: { var _tmp4 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (records) |r| { const _tmp6 = blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", r.d_year) catch unreachable; m.put("i_category_id", r.i_category_id) catch unreachable; m.put("i_category", r.i_category) catch unreachable; break :blk11 m; }; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(r) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(r) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", g.key.d_year) catch unreachable; m.put("i_category_id", g.key.i_category_id) catch unreachable; m.put("i_category", g.key.i_category) catch unreachable; m.put("sum_ss_ext_sales_price", _sum_int(blk13: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk13 _tmp3; })) catch unreachable; break :blk12 m; }) catch unreachable; } break :blk14 _tmp7.toOwnedSlice() catch unreachable; };
    result = base;
    _json(result);
    test_TPCDS_Q42_simplified();
}
