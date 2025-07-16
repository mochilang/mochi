const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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
var inventory: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q37_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "I1") catch unreachable; m.put("i_item_desc", "Item1") catch unreachable; m.put("i_current_price", 30) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    item = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "I1") catch unreachable; m.put("i_item_desc", "Item1") catch unreachable; m.put("i_current_price", 30) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(800))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_item_id", "I2") catch unreachable; m.put("i_item_desc", "Item2") catch unreachable; m.put("i_current_price", 60) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(801))) catch unreachable; break :blk2 m; }};
    inventory = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(200))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(300))) catch unreachable; break :blk4 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2000-01-15") catch unreachable; break :blk5 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk6 m; }};
    result = blk9: { var _tmp0 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp1 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (item) |i| { for (inventory) |inv| { if (!((i.i_item_sk == inv.inv_item_sk))) continue; for (date_dim) |d| { if (!((inv.inv_date_sk == d.d_date_sk))) continue; for (catalog_sales) |cs| { if (!((cs.cs_item_sk == i.i_item_sk))) continue; if (!(((((((i.i_current_price >= @as(i32,@intCast(20))) and (i.i_current_price <= @as(i32,@intCast(50)))) and (i.i_manufact_id >= @as(i32,@intCast(800)))) and (i.i_manufact_id <= @as(i32,@intCast(803)))) and (inv.inv_quantity_on_hand >= @as(i32,@intCast(100)))) and (inv.inv_quantity_on_hand <= @as(i32,@intCast(500)))))) continue; const _tmp2 = blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", i.i_item_id) catch unreachable; m.put("desc", i.i_item_desc) catch unreachable; m.put("price", i.i_current_price) catch unreachable; break :blk7 m; }; if (_tmp1.get(_tmp2)) |idx| { _tmp0.items[idx].Items.append(i) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(i) catch unreachable; _tmp0.append(g) catch unreachable; _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable; } } } } } var _tmp3 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp0.items) |g| { _tmp3.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.id) catch unreachable; m.put("i_item_desc", g.key.desc) catch unreachable; m.put("i_current_price", g.key.price) catch unreachable; break :blk8 m; }) catch unreachable; } break :blk9 _tmp3.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q37_simplified();
}
