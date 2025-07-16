const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
    return false;
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

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var zip_list: []const []const u8 = undefined;
var item_ids: []const []const u8 = undefined;
var qoy: i32 = undefined;
var year: i32 = undefined;
var base: []const std.AutoHashMap([]const u8, i32) = undefined;
var records: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q45_simplified() void {
    expect((records == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_zip", "85669") catch unreachable; m.put("sum_ws_sales_price", 50) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_zip", "99999") catch unreachable; m.put("sum_ws_sales_price", 30) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 50) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("bill_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 30) catch unreachable; break :blk3 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(2))) catch unreachable; break :blk5 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_zip", "85669") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_zip", "99999") catch unreachable; break :blk7 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "I1") catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_item_id", "I2") catch unreachable; break :blk9 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk10 m; }};
    zip_list = &[_][]const u8{"85669", "86197", "88274", "83405", "86475", "85392", "85460", "80348", "81792"};
    item_ids = &[_][]const u8{"I2"};
    qoy = @as(i32,@intCast(1));
    year = @as(i32,@intCast(2020));
    base = blk13: { var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (web_sales) |ws| { for (customer) |c| { if (!((ws.bill_customer_sk == c.c_customer_sk))) continue; for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; for (item) |i| { if (!((ws.item_sk == i.i_item_sk))) continue; for (date_dim) |d| { if (!((ws.sold_date_sk == d.d_date_sk))) continue; if (!(((((_contains_list_string(zip_list, _slice_string(ca.ca_zip, @as(i32,@intCast(0)), @as(i32,@intCast(5)), 1)) or _contains_list_string(item_ids, i.i_item_id))) and (d.d_qoy == qoy)) and (d.d_year == year)))) continue; const _tmp4 = ca.ca_zip; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ws) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ws) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_zip", g.key) catch unreachable; m.put("sum_ws_sales_price", _sum_int(blk12: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ws.sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk12 _tmp1; })) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk13 _tmp5.toOwnedSlice() catch unreachable; };
    records = base;
    _json(records);
    test_TPCDS_Q45_simplified();
}
