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
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q3_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("brand", "Brand1") catch unreachable; m.put("sum_agg", 10) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("brand_id", @as(i32,@intCast(2))) catch unreachable; m.put("brand", "Brand2") catch unreachable; m.put("sum_agg", 20) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; m.put("d_moy", @as(i32,@intCast(12))) catch unreachable; break :blk2 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 10) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_ext_sales_price", 20) catch unreachable; break :blk4 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(100))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_brand", "Brand1") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(100))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(2))) catch unreachable; m.put("i_brand", "Brand2") catch unreachable; break :blk6 m; }};
    result = blk10: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (date_dim) |dt| { for (store_sales) |ss| { if (!((dt.d_date_sk == ss.ss_sold_date_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; if (!(((i.i_manufact_id == @as(i32,@intCast(100))) and (dt.d_moy == @as(i32,@intCast(12)))))) continue; const _tmp4 = blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", dt.d_year) catch unreachable; m.put("brand_id", i.i_brand_id) catch unreachable; m.put("brand", i.i_brand) catch unreachable; break :blk7 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(dt) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(dt) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_year", g.key.d_year) catch unreachable; m.put("brand_id", g.key.brand_id) catch unreachable; m.put("brand", g.key.brand) catch unreachable; m.put("sum_agg", _sum_int(blk9: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss_ext_sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; })) catch unreachable; break :blk8 m; }) catch unreachable; } break :blk10 _tmp5.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q3_result();
}
