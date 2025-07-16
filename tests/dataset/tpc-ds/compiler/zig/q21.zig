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

const Inventory = struct {
    inv_item_sk: i32,
    inv_warehouse_sk: i32,
    inv_date_sk: i32,
    inv_quantity_on_hand: i32,
};

const Warehouse = struct {
    w_warehouse_sk: i32,
    w_warehouse_name: []const u8,
};

const Item = struct {
    i_item_sk: i32,
    i_item_id: []const u8,
};

const DateDim = struct {
    d_date_sk: i32,
    d_date: []const u8,
};

var inventory: []const std.AutoHashMap([]const u8, i32) = undefined;
var warehouse: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var before: []const std.AutoHashMap([]const u8, i32) = undefined;
var after: []const std.AutoHashMap([]const u8, i32) = undefined;
var joined: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q21_inventory_ratio() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("w_warehouse_name", "Main") catch unreachable; m.put("i_item_id", "ITEM1") catch unreachable; m.put("inv_before", @as(i32,@intCast(30))) catch unreachable; m.put("inv_after", @as(i32,@intCast(40))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    inventory = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(30))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(40))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(20))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(20))) catch unreachable; break :blk4 m; }};
    warehouse = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("w_warehouse_name", "Main") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_warehouse_sk", @as(i32,@intCast(2))) catch unreachable; m.put("w_warehouse_name", "Backup") catch unreachable; break :blk6 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "ITEM1") catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_item_id", "ITEM2") catch unreachable; break :blk8 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2000-03-01") catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_date", "2000-03-20") catch unreachable; break :blk10 m; }};
    before = blk14: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (inventory) |inv| { for (date_dim) |d| { if (!((inv.inv_date_sk == d.d_date_sk))) continue; if (!((d.d_date < "2000-03-15"))) continue; const _tmp4 = blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w", inv.inv_warehouse_sk) catch unreachable; m.put("i", inv.inv_item_sk) catch unreachable; break :blk11 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(inv) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(inv) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w", g.key.w) catch unreachable; m.put("i", g.key.i) catch unreachable; m.put("qty", _sum_int(blk13: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.inv_quantity_on_hand) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk13 _tmp1; })) catch unreachable; break :blk12 m; }) catch unreachable; } break :blk14 _tmp5.toOwnedSlice() catch unreachable; };
    after = blk18: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (inventory) |inv| { for (date_dim) |d| { if (!((inv.inv_date_sk == d.d_date_sk))) continue; if (!((d.d_date >= "2000-03-15"))) continue; const _tmp10 = blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w", inv.inv_warehouse_sk) catch unreachable; m.put("i", inv.inv_item_sk) catch unreachable; break :blk15 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(inv) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(inv) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w", g.key.w) catch unreachable; m.put("i", g.key.i) catch unreachable; m.put("qty", _sum_int(blk17: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.inv_quantity_on_hand) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk17 _tmp7; })) catch unreachable; break :blk16 m; }) catch unreachable; } break :blk18 _tmp11.toOwnedSlice() catch unreachable; };
    joined = blk20: { var _tmp12 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (before) |b| { for (after) |a| { if (!(((b.w == a.w) and (b.i == a.i)))) continue; for (warehouse) |w| { if (!((w.w_warehouse_sk == b.w))) continue; for (item) |it| { if (!((it.i_item_sk == b.i))) continue; _tmp12.append(blk19: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_name", w.w_warehouse_name) catch unreachable; m.put("i_id", it.i_item_id) catch unreachable; m.put("before_qty", b.qty) catch unreachable; m.put("after_qty", a.qty) catch unreachable; m.put("ratio", (a.qty / b.qty)) catch unreachable; break :blk19 m; }) catch unreachable; } } } } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk20 _tmp13; };
    result = blk22: { var _tmp14 = std.ArrayList(struct { item: std.AutoHashMap([]const u8, i32), key: []const i32 }).init(std.heap.page_allocator); for (joined) |r| { if (!(((r.ratio >= ((2 / 3))) and (r.ratio <= ((3 / 2)))))) continue; _tmp14.append(.{ .item = blk21: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_warehouse_name", r.w_name) catch unreachable; m.put("i_item_id", r.i_id) catch unreachable; m.put("inv_before", r.before_qty) catch unreachable; m.put("inv_after", r.after_qty) catch unreachable; break :blk21 m; }, .key = &[_]i32{r.w_name, r.i_id} }) catch unreachable; } for (0.._tmp14.items.len) |i| { for (i+1.._tmp14.items.len) |j| { if (_tmp14.items[j].key < _tmp14.items[i].key) { const t = _tmp14.items[i]; _tmp14.items[i] = _tmp14.items[j]; _tmp14.items[j] = t; } } } var _tmp15 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp14.items) |p| { _tmp15.append(p.item) catch unreachable; } const _tmp16 = _tmp15.toOwnedSlice() catch unreachable; break :blk22 _tmp16; };
    _json(result);
    test_TPCDS_Q21_inventory_ratio();
}
