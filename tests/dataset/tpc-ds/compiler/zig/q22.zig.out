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

const Inventory = struct {
    inv_item_sk: i32,
    inv_date_sk: i32,
    inv_quantity_on_hand: i32,
};

const DateDim = struct {
    d_date_sk: i32,
    d_month_seq: i32,
};

const Item = struct {
    i_item_sk: i32,
    i_product_name: []const u8,
    i_brand: []const u8,
    i_class: []const u8,
    i_category: []const u8,
};

var inventory: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var qoh: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q22_average_inventory() void {
    expect((qoh == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_product_name", "Prod1") catch unreachable; m.put("i_brand", "Brand1") catch unreachable; m.put("i_class", "Class1") catch unreachable; m.put("i_category", "Cat1") catch unreachable; m.put(qoh, 15) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_product_name", "Prod2") catch unreachable; m.put("i_brand", "Brand2") catch unreachable; m.put("i_class", "Class2") catch unreachable; m.put("i_category", "Cat2") catch unreachable; m.put(qoh, 50) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    inventory = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(10))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(20))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(10))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(4))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(20))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(50))) catch unreachable; break :blk6 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(0))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1))) catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(2))) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(4))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(3))) catch unreachable; break :blk10 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_product_name", "Prod1") catch unreachable; m.put("i_brand", "Brand1") catch unreachable; m.put("i_class", "Class1") catch unreachable; m.put("i_category", "Cat1") catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_product_name", "Prod2") catch unreachable; m.put("i_brand", "Brand2") catch unreachable; m.put("i_class", "Class2") catch unreachable; m.put("i_category", "Cat2") catch unreachable; break :blk12 m; }};
    qoh = blk16: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (inventory) |inv| { for (date_dim) |d| { if (!((inv.inv_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!((inv.inv_item_sk == i.i_item_sk))) continue; if (!(((d.d_month_seq >= @as(i32,@intCast(0))) and (d.d_month_seq <= @as(i32,@intCast(11)))))) continue; const _tmp4 = blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("product_name", i.i_product_name) catch unreachable; m.put("brand", i.i_brand) catch unreachable; m.put("class", i.i_class) catch unreachable; m.put("category", i.i_category) catch unreachable; break :blk13 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(inv) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(inv) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_product_name", g.key.product_name) catch unreachable; m.put("i_brand", g.key.brand) catch unreachable; m.put("i_class", g.key.class) catch unreachable; m.put("i_category", g.key.category) catch unreachable; m.put(qoh, _avg_int(blk15: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.inv_quantity_on_hand) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk15 _tmp1; })) catch unreachable; break :blk14 m; }) catch unreachable; } break :blk16 _tmp5.toOwnedSlice() catch unreachable; };
    _json(qoh);
    test_TPCDS_Q22_average_inventory();
}
