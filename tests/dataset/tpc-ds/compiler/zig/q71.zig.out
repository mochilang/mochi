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
var time_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var month: i32 = undefined;
var year: i32 = undefined;
var union_sales: []const i32 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q71_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_brand_id", @as(i32,@intCast(10))) catch unreachable; m.put("i_brand", "BrandA") catch unreachable; m.put("t_hour", @as(i32,@intCast(18))) catch unreachable; m.put("t_minute", @as(i32,@intCast(0))) catch unreachable; m.put("ext_price", 200) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_brand_id", @as(i32,@intCast(20))) catch unreachable; m.put("i_brand", "BrandB") catch unreachable; m.put("t_hour", @as(i32,@intCast(8))) catch unreachable; m.put("t_minute", @as(i32,@intCast(30))) catch unreachable; m.put("ext_price", 150) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_brand_id", @as(i32,@intCast(10))) catch unreachable; m.put("i_brand", "BrandA") catch unreachable; m.put("t_hour", @as(i32,@intCast(8))) catch unreachable; m.put("t_minute", @as(i32,@intCast(30))) catch unreachable; m.put("ext_price", 100) catch unreachable; break :blk2 m; }}));
}

pub fn main() void {
    item = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(10))) catch unreachable; m.put("i_brand", "BrandA") catch unreachable; m.put("i_manager_id", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(20))) catch unreachable; m.put("i_brand", "BrandB") catch unreachable; m.put("i_manager_id", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    time_dim = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("t_hour", @as(i32,@intCast(8))) catch unreachable; m.put("t_minute", @as(i32,@intCast(30))) catch unreachable; m.put("t_meal_time", "breakfast") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(2))) catch unreachable; m.put("t_hour", @as(i32,@intCast(18))) catch unreachable; m.put("t_minute", @as(i32,@intCast(0))) catch unreachable; m.put("t_meal_time", "dinner") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(3))) catch unreachable; m.put("t_hour", @as(i32,@intCast(12))) catch unreachable; m.put("t_minute", @as(i32,@intCast(0))) catch unreachable; m.put("t_meal_time", "lunch") catch unreachable; break :blk7 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_moy", @as(i32,@intCast(12))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk8 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("ws_ext_sales_price", 100) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_time_sk", @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk10: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("cs_ext_sales_price", 200) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_time_sk", @as(i32,@intCast(2))) catch unreachable; break :blk10 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk11: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("ss_ext_sales_price", 150) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_time_sk", @as(i32,@intCast(1))) catch unreachable; break :blk11 m; }};
    month = @as(i32,@intCast(12));
    year = @as(i32,@intCast(1998));
    union_sales = concat(blk13: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { for (date_dim) |d| { if (!((d.d_date_sk == ws.ws_sold_date_sk))) continue; if (!(((d.d_moy == month) and (d.d_year == year)))) continue; _tmp0.append(blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ext_price", ws.ws_ext_sales_price) catch unreachable; m.put("item_sk", ws.ws_item_sk) catch unreachable; m.put("time_sk", ws.ws_sold_time_sk) catch unreachable; break :blk12 m; }) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk13 _tmp1; }, blk15: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (date_dim) |d| { if (!((d.d_date_sk == cs.cs_sold_date_sk))) continue; if (!(((d.d_moy == month) and (d.d_year == year)))) continue; _tmp2.append(blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ext_price", cs.cs_ext_sales_price) catch unreachable; m.put("item_sk", cs.cs_item_sk) catch unreachable; m.put("time_sk", cs.cs_sold_time_sk) catch unreachable; break :blk14 m; }) catch unreachable; } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk15 _tmp3; }, blk17: { var _tmp4 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; if (!(((d.d_moy == month) and (d.d_year == year)))) continue; _tmp4.append(blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ext_price", ss.ss_ext_sales_price) catch unreachable; m.put("item_sk", ss.ss_item_sk) catch unreachable; m.put("time_sk", ss.ss_sold_time_sk) catch unreachable; break :blk16 m; }) catch unreachable; } } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk17 _tmp5; });
    result = blk21: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (item) |i| { for (union_sales) |s| { if (!((s.item_sk == i.i_item_sk))) continue; for (time_dim) |t| { if (!((t.t_time_sk == s.time_sk))) continue; if (!(((i.i_manager_id == @as(i32,@intCast(1))) and ((std.mem.eql(u8, t.t_meal_time, "breakfast") or std.mem.eql(u8, t.t_meal_time, "dinner")))))) continue; const _tmp10 = blk18: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("brand_id", i.i_brand_id) catch unreachable; m.put("brand", i.i_brand) catch unreachable; m.put("t_hour", t.t_hour) catch unreachable; m.put("t_minute", t.t_minute) catch unreachable; break :blk18 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(i) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(i) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk19: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_brand_id", g.key.brand_id) catch unreachable; m.put("i_brand", g.key.brand) catch unreachable; m.put("t_hour", g.key.t_hour) catch unreachable; m.put("t_minute", g.key.t_minute) catch unreachable; m.put("ext_price", _sum_int(blk20: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.s.ext_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk20 _tmp7; })) catch unreachable; break :blk19 m; }) catch unreachable; } break :blk21 _tmp11.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q71_simplified();
}
