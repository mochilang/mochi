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

const CatalogSale = struct {
    cs_order_number: i32,
    cs_ship_date_sk: i32,
    cs_ship_addr_sk: i32,
    cs_call_center_sk: i32,
    cs_warehouse_sk: i32,
    cs_ext_ship_cost: f64,
    cs_net_profit: f64,
};

const DateDim = struct {
    d_date_sk: i32,
    d_date: []const u8,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_state: []const u8,
};

const CallCenter = struct {
    cc_call_center_sk: i32,
    cc_county: []const u8,
};

const CatalogReturn = struct {
    cr_order_number: i32,
};

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var call_center: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_returns: []const i32 = undefined;
var filtered: []const std.AutoHashMap([]const u8, i32) = undefined;

fn distinct(xs: []const i32) []const i32 {
    var out = std.ArrayList(i32).init(std.heap.page_allocator);
    for ("xs") |x| {
        if (!contains(out, "x")) {
            out = append(out, "x");
        }
    }
    return out.items;
}

fn test_TPCDS_Q16_shipping() void {
    expect((filtered == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order_count", @as(i32,@intCast(1))) catch unreachable; m.put("total_shipping_cost", 5) catch unreachable; m.put("total_net_profit", 20) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_order_number", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ext_ship_cost", 5) catch unreachable; m.put("cs_net_profit", 20) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_order_number", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ship_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_call_center_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_warehouse_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_ext_ship_cost", 0) catch unreachable; m.put("cs_net_profit", 0) catch unreachable; break :blk2 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2000-03-01") catch unreachable; break :blk3 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk4 m; }};
    call_center = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cc_call_center_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cc_county", "CountyA") catch unreachable; break :blk5 m; }};
    catalog_returns = &[_]i32{};
    filtered = blk13: { var _tmp10 = std.ArrayList(struct { key: std.AutoHashMap(i32, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp11 = std.AutoHashMap(std.AutoHashMap(i32, i32), usize).init(std.heap.page_allocator); for (catalog_sales) |cs1| { for (date_dim) |d| { if (!((((cs1.cs_ship_date_sk == d.d_date_sk) and (d.d_date >= "2000-03-01")) and (d.d_date <= "2000-04-30")))) continue; for (customer_address) |ca| { if (!(((cs1.cs_ship_addr_sk == ca.ca_address_sk) and std.mem.eql(u8, ca.ca_state, "CA")))) continue; for (call_center) |cc| { if (!(((cs1.cs_call_center_sk == cc.cc_call_center_sk) and std.mem.eql(u8, cc.cc_county, "CountyA")))) continue; if (!((exists(blk11: { var _tmp6 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs2| { if (!(((cs1.cs_order_number == cs2.cs_order_number) and (cs1.cs_warehouse_sk != cs2.cs_warehouse_sk)))) continue; _tmp6.append(cs2) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk11 _tmp7; }) and (exists(blk12: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (catalog_returns) |cr| { if (!((cs1.cs_order_number == cr.cr_order_number))) continue; _tmp8.append(cr) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk12 _tmp9; }) == false)))) continue; const _tmp12 = blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); break :blk6 m; }; if (_tmp11.get(_tmp12)) |idx| { _tmp10.items[idx].Items.append(cs1) catch unreachable; } else { var g = struct { key: std.AutoHashMap(i32, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp12, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cs1) catch unreachable; _tmp10.append(g) catch unreachable; _tmp11.put(_tmp12, _tmp10.items.len - 1) catch unreachable; } } } } } var _tmp13 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp13.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("order_count", (distinct(blk8: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.cs_order_number) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk8 _tmp1; })).len) catch unreachable; m.put("total_shipping_cost", _sum_int(blk9: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.cs_ext_ship_cost) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk9 _tmp3; })) catch unreachable; m.put("total_net_profit", _sum_int(blk10: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.cs_net_profit) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk10 _tmp5; })) catch unreachable; break :blk7 m; }) catch unreachable; } break :blk13 _tmp13.toOwnedSlice() catch unreachable; };
    _json(filtered);
    test_TPCDS_Q16_shipping();
}
