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

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var zip_list: []const []const u8 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q8_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("s_store_name", "Store1") catch unreachable; m.put("net_profit", 10) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_profit", 10) catch unreachable; break :blk1 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk2 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_store_name", "Store1") catch unreachable; m.put("s_zip", "12345") catch unreachable; break :blk3 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_zip", "12345") catch unreachable; break :blk4 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_preferred_cust_flag", "Y") catch unreachable; break :blk5 m; }};
    reverse(_slice_string("zip", @as(i32,@intCast(0)), @as(i32,@intCast(2)), 1));
    zip_list = &[_][]const u8{"12345"};
    result = blk8: { var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((((ss.ss_sold_date_sk == d.d_date_sk) and (d.d_qoy == @as(i32,@intCast(1)))) and (d.d_year == @as(i32,@intCast(1998)))))) continue; for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; for (customer_address) |ca| { if (!((_slice_string(s.s_zip, @as(i32,@intCast(0)), @as(i32,@intCast(2)), 1) == _slice_string(ca.ca_zip, @as(i32,@intCast(0)), @as(i32,@intCast(2)), 1)))) continue; for (customer) |c| { if (!(((ca.ca_address_sk == c.c_current_addr_sk) and std.mem.eql(u8, c.c_preferred_cust_flag, "Y")))) continue; if (!(_contains_list_string(zip_list, _slice_string(ca.ca_zip, @as(i32,@intCast(0)), @as(i32,@intCast(5)), 1)))) continue; const _tmp4 = s.s_store_name; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_name", g.key) catch unreachable; m.put("net_profit", _sum_int(blk7: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss.ss_net_profit) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; })) catch unreachable; break :blk6 m; }) catch unreachable; } break :blk8 _tmp5.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q8_result();
}
