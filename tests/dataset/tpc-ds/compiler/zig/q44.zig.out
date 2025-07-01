const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += @floatFromInt(it); }
    return sum / @as(f64, @floatFromInt(v.len));
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
var grouped_base: []const std.AutoHashMap([]const u8, i32) = undefined;
var grouped: []const std.AutoHashMap([]const u8, i32) = undefined;
var best: i32 = undefined;
var worst: i32 = undefined;
var best_name: i32 = undefined;
var worst_name: i32 = undefined;
var result: std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q44_simplified() void {
    expect((result == blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("best_performing", "ItemA") catch unreachable; m.put("worst_performing", "ItemB") catch unreachable; break :blk0 m; }));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_profit", 5) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_profit", 5) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_profit", -1) catch unreachable; break :blk3 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_product_name", "ItemA") catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_product_name", "ItemB") catch unreachable; break :blk5 m; }};
    grouped_base = (blk8: { var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (store_sales) |ss| { const _tmp4 = ss.ss_item_sk; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item_sk", g.key) catch unreachable; m.put("avg_profit", _avg_int(blk7: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss_net_profit) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; })) catch unreachable; break :blk6 m; }) catch unreachable; } break :blk8 _tmp5.toOwnedSlice() catch unreachable; });
    grouped = grouped_base;
    best = first(blk9: { var _tmp6 = std.ArrayList(struct { item: std.AutoHashMap([]const u8, i32), key: i32 }).init(std.heap.page_allocator); for (grouped) |x| { _tmp6.append(.{ .item = x, .key = -x.avg_profit }) catch unreachable; } for (0.._tmp6.items.len) |i| { for (i+1.._tmp6.items.len) |j| { if (_tmp6.items[j].key < _tmp6.items[i].key) { const t = _tmp6.items[i]; _tmp6.items[i] = _tmp6.items[j]; _tmp6.items[j] = t; } } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp6.items) |p| { _tmp7.append(p.item) catch unreachable; } const _tmp8 = _tmp7.toOwnedSlice() catch unreachable; break :blk9 _tmp8; });
    worst = first(blk10: { var _tmp9 = std.ArrayList(struct { item: std.AutoHashMap([]const u8, i32), key: i32 }).init(std.heap.page_allocator); for (grouped) |x| { _tmp9.append(.{ .item = x, .key = x.avg_profit }) catch unreachable; } for (0.._tmp9.items.len) |i| { for (i+1.._tmp9.items.len) |j| { if (_tmp9.items[j].key < _tmp9.items[i].key) { const t = _tmp9.items[i]; _tmp9.items[i] = _tmp9.items[j]; _tmp9.items[j] = t; } } } var _tmp10 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp9.items) |p| { _tmp10.append(p.item) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk10 _tmp11; });
    best_name = first(blk11: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (item) |i| { if (!((i.i_item_sk == best.item_sk))) continue; _tmp12.append(i.i_product_name) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk11 _tmp13; });
    worst_name = first(blk12: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (item) |i| { if (!((i.i_item_sk == worst.item_sk))) continue; _tmp14.append(i.i_product_name) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk12 _tmp15; });
    result = blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("best_performing", best_name) catch unreachable; m.put("worst_performing", worst_name) catch unreachable; break :blk13 m; };
    _json(result);
    test_TPCDS_Q44_simplified();
}
