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
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q36_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_category", "Books") catch unreachable; m.put("i_class", "C1") catch unreachable; m.put("gross_margin", 0.2) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_category", "Books") catch unreachable; m.put("i_class", "C2") catch unreachable; m.put("gross_margin", 0.25) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_category", "Electronics") catch unreachable; m.put("i_class", "C3") catch unreachable; m.put("gross_margin", 0.2) catch unreachable; break :blk2 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; m.put("ss_net_profit", 20) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 200) catch unreachable; m.put("ss_net_profit", 50) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(3))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 150) catch unreachable; m.put("ss_net_profit", 30) catch unreachable; break :blk5 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "Books") catch unreachable; m.put("i_class", "C1") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_category", "Books") catch unreachable; m.put("i_class", "C2") catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(3))) catch unreachable; m.put("i_category", "Electronics") catch unreachable; m.put("i_class", "C3") catch unreachable; break :blk8 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_state", "A") catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(2))) catch unreachable; m.put("s_state", "B") catch unreachable; break :blk10 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk11 m; }};
    result = blk16: { var _tmp4 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; if (!(((d.d_year == @as(i32,@intCast(2000))) and ((std.mem.eql(u8, s.s_state, "A") or std.mem.eql(u8, s.s_state, "B")))))) continue; const _tmp6 = blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("category", i.i_category) catch unreachable; m.put("class", i.i_class) catch unreachable; break :blk12 m; }; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } } } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_category", g.key.category) catch unreachable; m.put("i_class", g.key.class) catch unreachable; m.put("gross_margin", (_sum_int(blk14: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss_net_profit) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk14 _tmp1; }) / _sum_int(blk15: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.ss_ext_sales_price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk15 _tmp3; }))) catch unreachable; break :blk13 m; }) catch unreachable; } break :blk16 _tmp7.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q36_simplified();
}
