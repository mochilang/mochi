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

const StoreSale = struct {
    ss_sold_date_sk: i32,
    ss_item_sk: i32,
    ss_customer_sk: i32,
    ss_store_sk: i32,
    ss_ext_sales_price: f64,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
    d_moy: i32,
};

const Item = struct {
    i_item_sk: i32,
    i_brand_id: i32,
    i_brand: []const u8,
    i_manufact_id: i32,
    i_manufact: []const u8,
    i_manager_id: i32,
};

const Customer = struct {
    c_customer_sk: i32,
    c_current_addr_sk: i32,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_zip: []const u8,
};

const Store = struct {
    s_store_sk: i32,
    s_zip: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q19_brand() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_brand", "B1") catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact", "M1") catch unreachable; m.put("ext_price", 100) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; break :blk1 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; m.put("d_moy", @as(i32,@intCast(11))) catch unreachable; break :blk2 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_brand_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_brand", "B1") catch unreachable; m.put("i_manufact_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_manufact", "M1") catch unreachable; m.put("i_manager_id", @as(i32,@intCast(10))) catch unreachable; break :blk3 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_zip", "11111") catch unreachable; break :blk5 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_zip", "99999") catch unreachable; break :blk6 m; }};
    result = blk10: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (date_dim) |d| { for (store_sales) |ss| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!(((ss.ss_item_sk == i.i_item_sk) and (i.i_manager_id == @as(i32,@intCast(10)))))) continue; for (customer) |c| { if (!((ss.ss_customer_sk == c.c_customer_sk))) continue; for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; for (store) |s| { if (!(((ss.ss_store_sk == s.s_store_sk) and (_slice_string(ca.ca_zip, @as(i32,@intCast(0)), @as(i32,@intCast(5)), 1) != _slice_string(s.s_zip, @as(i32,@intCast(0)), @as(i32,@intCast(5)), 1))))) continue; if (!(((d.d_moy == @as(i32,@intCast(11))) and (d.d_year == @as(i32,@intCast(1999)))))) continue; const _tmp4 = blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("brand", i.i_brand) catch unreachable; m.put("brand_id", i.i_brand_id) catch unreachable; m.put("man_id", i.i_manufact_id) catch unreachable; m.put("man", i.i_manufact) catch unreachable; break :blk7 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(d) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(d) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_brand", g.key.brand) catch unreachable; m.put("i_brand_id", g.key.brand_id) catch unreachable; m.put("i_manufact_id", g.key.man_id) catch unreachable; m.put("i_manufact", g.key.man) catch unreachable; m.put("ext_price", _sum_int(blk9: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss_ext_sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; })) catch unreachable; break :blk8 m; }) catch unreachable; } break :blk10 _tmp5.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q19_brand();
}
