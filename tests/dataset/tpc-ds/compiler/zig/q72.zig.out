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

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var inventory: []const std.AutoHashMap([]const u8, i32) = undefined;
var warehouse: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q72_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_desc", "ItemA") catch unreachable; m.put("w_warehouse_name", "Main") catch unreachable; m.put("d_week_seq", @as(i32,@intCast(10))) catch unreachable; m.put("no_promo", @as(i32,@intCast(1))) catch unreachable; m.put("promo", @as(i32,@intCast(0))) catch unreachable; m.put("total_cnt", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_order_number", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cs_bill_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_promo_sk", 0) catch unreachable; break :blk1 m; }};
    inventory = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("inv_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("inv_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("inv_quantity_on_hand", @as(i32,@intCast(0))) catch unreachable; break :blk2 m; }};
    warehouse = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("w_warehouse_name", "Main") catch unreachable; break :blk3 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_desc", "ItemA") catch unreachable; break :blk4 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_marital_status", "M") catch unreachable; break :blk5 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_buy_potential", "5001-10000") catch unreachable; break :blk6 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(10))) catch unreachable; m.put("d_date", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(10))) catch unreachable; m.put("d_date", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(10))) catch unreachable; m.put("d_date", @as(i32,@intCast(7))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk9 m; }};
    result = blk14: { var _tmp4 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (inventory) |inv| { if (!((inv.inv_item_sk == cs.cs_item_sk))) continue; for (warehouse) |w| { if (!((w.w_warehouse_sk == inv.inv_warehouse_sk))) continue; for (item) |i| { if (!((i.i_item_sk == cs.cs_item_sk))) continue; for (customer_demographics) |cd| { if (!((cd.cd_demo_sk == cs.cs_bill_cdemo_sk))) continue; for (household_demographics) |hd| { if (!((hd.hd_demo_sk == cs.cs_bill_hdemo_sk))) continue; for (date_dim) |d1| { if (!((d1.d_date_sk == cs.cs_sold_date_sk))) continue; for (date_dim) |d2| { if (!((d2.d_date_sk == inv.inv_date_sk))) continue; for (date_dim) |d3| { if (!((d3.d_date_sk == cs.cs_ship_date_sk))) continue; if (!(((((((d1.d_week_seq == d2.d_week_seq) and (inv.inv_quantity_on_hand < cs.cs_quantity)) and (d3.d_date > (d1.d_date + @as(i32,@intCast(5))))) and std.mem.eql(u8, hd.hd_buy_potential, "5001-10000")) and (d1.d_year == @as(i32,@intCast(2000)))) and std.mem.eql(u8, cd.cd_marital_status, "M")))) continue; const _tmp6 = blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item_desc", i.i_item_desc) catch unreachable; m.put(warehouse, w.w_warehouse_name) catch unreachable; m.put("week_seq", d1.d_week_seq) catch unreachable; break :blk10 m; }; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(cs) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cs) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } } } } } } } } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_desc", g.key.item_desc) catch unreachable; m.put("w_warehouse_name", g.key.warehouse) catch unreachable; m.put("d_week_seq", g.key.week_seq) catch unreachable; m.put("no_promo", (blk12: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!((x.cs_promo_sk == 0))) continue; _tmp0.append(x) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk12 _tmp1; }).len) catch unreachable; m.put("promo", (blk13: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!((x.cs_promo_sk != 0))) continue; _tmp2.append(x) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk13 _tmp3; }).len) catch unreachable; m.put("total_cnt", (g.Items.len)) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk14 _tmp7.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q72_simplified();
}
