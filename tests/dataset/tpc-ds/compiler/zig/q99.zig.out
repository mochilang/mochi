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

fn _slice_string(s: []const u8, start: i32, end: i32, step: i32) []const u8 {
    var sidx = start;
    var eidx = end;
    var stp = step;
    const n: i32 = @as(i32, @intCast(s.len));
    if (sidx < 0) sidx += n;
    if (eidx < 0) eidx += n;
    if (stp == 0) stp = 1;
    if (sidx < 0) sidx = 0;
    if (eidx > n) eidx = n;
    if (stp > 0 and eidx < sidx) eidx = sidx;
    if (stp < 0 and eidx > sidx) eidx = sidx;
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = sidx;
    while ((stp > 0 and i < eidx) or (stp < 0 and i > eidx)) : (i += stp) {
        res.append(s[@as(usize, @intCast(i))]) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const CatalogSale = struct {
    cs_ship_date_sk: i32,
    cs_sold_date_sk: i32,
    cs_warehouse_sk: i32,
    cs_ship_mode_sk: i32,
    cs_call_center_sk: i32,
};

const Warehouse = struct {
    w_warehouse_sk: i32,
    w_warehouse_name: []const u8,
};

const ShipMode = struct {
    sm_ship_mode_sk: i32,
    sm_type: []const u8,
};

const CallCenter = struct {
    cc_call_center_sk: i32,
    cc_name: []const u8,
};

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var warehouse: []const std.AutoHashMap([]const u8, i32) = undefined;
var ship_mode: []const std.AutoHashMap([]const u8, i32) = undefined;
var call_center: []const std.AutoHashMap([]const u8, i32) = undefined;
var grouped: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q99_buckets() void {
    expect((grouped == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(warehouse, "Warehouse1") catch unreachable; m.put("sm_type", "EXP") catch unreachable; m.put("cc_name", "CC1") catch unreachable; m.put("d30", @as(i32,@intCast(1))) catch unreachable; m.put("d60", @as(i32,@intCast(1))) catch unreachable; m.put("d90", @as(i32,@intCast(1))) catch unreachable; m.put("d120", @as(i32,@intCast(1))) catch unreachable; m.put("dmore", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_ship_date_sk", @as(i32,@intCast(31))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_mode_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_ship_date_sk", @as(i32,@intCast(51))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_mode_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_ship_date_sk", @as(i32,@intCast(71))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_mode_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_ship_date_sk", @as(i32,@intCast(101))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_mode_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_ship_date_sk", @as(i32,@intCast(131))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_mode_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; break :blk5 m; }};
    warehouse = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("w_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("w_warehouse_name", "Warehouse1") catch unreachable; break :blk6 m; }};
    ship_mode = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sm_ship_mode_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sm_type", "EXP") catch unreachable; break :blk7 m; }};
    call_center = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cc_call_center_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cc_name", "CC1") catch unreachable; break :blk8 m; }};
    grouped = blk16: { var _tmp10 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp11 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (warehouse) |w| { if (!((cs.cs_warehouse_sk == w.w_warehouse_sk))) continue; for (ship_mode) |sm| { if (!((cs.cs_ship_mode_sk == sm.sm_ship_mode_sk))) continue; for (call_center) |cc| { if (!((cs.cs_call_center_sk == cc.cc_call_center_sk))) continue; const _tmp12 = blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(warehouse, _slice_string(w.w_warehouse_name, @as(i32,@intCast(0)), @as(i32,@intCast(20)), 1)) catch unreachable; m.put("sm_type", sm.sm_type) catch unreachable; m.put("cc_name", cc.cc_name) catch unreachable; break :blk9 m; }; if (_tmp11.get(_tmp12)) |idx| { _tmp10.items[idx].Items.append(cs) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp12, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cs) catch unreachable; _tmp10.append(g) catch unreachable; _tmp11.put(_tmp12, _tmp10.items.len - 1) catch unreachable; } } } } } var _tmp13 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp13.append(blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(warehouse, g.key.warehouse) catch unreachable; m.put("sm_type", g.key.sm_type) catch unreachable; m.put("cc_name", g.key.cc_name) catch unreachable; m.put("d30", (blk11: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(((x.cs_ship_date_sk - x.cs_sold_date_sk) <= @as(i32,@intCast(30))))) continue; _tmp0.append(x) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; }).len) catch unreachable; m.put("d60", (blk12: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!((((x.cs_ship_date_sk - x.cs_sold_date_sk) > @as(i32,@intCast(30))) and ((x.cs_ship_date_sk - x.cs_sold_date_sk) <= @as(i32,@intCast(60)))))) continue; _tmp2.append(x) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk12 _tmp3; }).len) catch unreachable; m.put("d90", (blk13: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!((((x.cs_ship_date_sk - x.cs_sold_date_sk) > @as(i32,@intCast(60))) and ((x.cs_ship_date_sk - x.cs_sold_date_sk) <= @as(i32,@intCast(90)))))) continue; _tmp4.append(x) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk13 _tmp5; }).len) catch unreachable; m.put("d120", (blk14: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!((((x.cs_ship_date_sk - x.cs_sold_date_sk) > @as(i32,@intCast(90))) and ((x.cs_ship_date_sk - x.cs_sold_date_sk) <= @as(i32,@intCast(120)))))) continue; _tmp6.append(x) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk14 _tmp7; }).len) catch unreachable; m.put("dmore", (blk15: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(((x.cs_ship_date_sk - x.cs_sold_date_sk) > @as(i32,@intCast(120))))) continue; _tmp8.append(x) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk15 _tmp9; }).len) catch unreachable; break :blk10 m; }) catch unreachable; } break :blk16 _tmp13.toOwnedSlice() catch unreachable; };
    _json(grouped);
    test_TPCDS_Q99_buckets();
}
