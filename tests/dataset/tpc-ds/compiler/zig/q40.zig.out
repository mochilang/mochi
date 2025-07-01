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

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var warehouse: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var sales_date: []const u8 = undefined;
var records: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q40_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("w_state", "CA") catch unreachable; m.put("i_item_id", "I1") catch unreachable; m.put("sales_before", 100) catch unreachable; m.put("sales_after", 0) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order", @as(i32,@intCast(1))) catch unreachable; m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("price", 100) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order", @as(i32,@intCast(2))) catch unreachable; m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("price", 150) catch unreachable; break :blk2 m; }};
    catalog_returns = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order", @as(i32,@intCast(2))) catch unreachable; m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("refunded", 150) catch unreachable; break :blk3 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("item_id", "I1") catch unreachable; m.put("current_price", 1.2) catch unreachable; break :blk4 m; }};
    warehouse = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("state", "CA") catch unreachable; break :blk5 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("date", "2020-01-10") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("date", "2020-01-20") catch unreachable; break :blk7 m; }};
    sales_date = "2020-01-15";
    records = blk9: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (catalog_returns) |cr| { if (!(((cs.order == cr.order) and (cs.item_sk == cr.item_sk)))) continue; for (warehouse) |w| { if (!((cs.warehouse_sk == w.warehouse_sk))) continue; for (item) |i| { if (!((cs.item_sk == i.item_sk))) continue; for (date_dim) |d| { if (!((cs.date_sk == d.date_sk))) continue; if (!(((i.current_price >= 0.99) and (i.current_price <= 1.49)))) continue; _tmp0.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_state", w.state) catch unreachable; m.put("i_item_id", i.item_id) catch unreachable; m.put("sold_date", d.date) catch unreachable; m.put("net", (cs.price - (if ((cr == 0)) (0) else (cr.refunded)))) catch unreachable; break :blk8 m; }) catch unreachable; } } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; };
    result = blk14: { var _tmp6 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp7 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (records) |r| { const _tmp8 = blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_state", r.w_state) catch unreachable; m.put("i_item_id", r.i_item_id) catch unreachable; break :blk10 m; }; if (_tmp7.get(_tmp8)) |idx| { _tmp6.items[idx].Items.append(r) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp8, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(r) catch unreachable; _tmp6.append(g) catch unreachable; _tmp7.put(_tmp8, _tmp6.items.len - 1) catch unreachable; } } var _tmp9 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp6.items) |g| { _tmp9.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_state", g.key.w_state) catch unreachable; m.put("i_item_id", g.key.i_item_id) catch unreachable; m.put("sales_before", _sum_int(blk12: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(if ((x.sold_date < sales_date)) (x.net) else (0)) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk12 _tmp3; })) catch unreachable; m.put("sales_after", _sum_int(blk13: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(if ((x.sold_date >= sales_date)) (x.net) else (0)) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk13 _tmp5; })) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk14 _tmp9.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q40_simplified();
}
