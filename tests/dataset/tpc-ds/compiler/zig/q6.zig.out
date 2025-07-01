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

var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var target_month_seq: i32 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q6_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("state", "CA") catch unreachable; m.put("cnt", @as(i32,@intCast(10))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_state", "CA") catch unreachable; m.put("ca_zip", "12345") catch unreachable; break :blk1 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk2 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk10 m; }, blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk12 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; m.put("d_moy", @as(i32,@intCast(5))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(120))) catch unreachable; break :blk13 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_category", "A") catch unreachable; m.put("i_current_price", 100) catch unreachable; break :blk14 m; }, blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_category", "A") catch unreachable; m.put("i_current_price", 50) catch unreachable; break :blk15 m; }};
    target_month_seq = max(blk16: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (date_dim) |d| { if (!(((d.d_year == @as(i32,@intCast(1999))) and (d.d_moy == @as(i32,@intCast(5)))))) continue; _tmp0.append(d.d_month_seq) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk16 _tmp1; });
    result = blk19: { var _tmp4 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (customer_address) |a| { for (customer) |c| { if (!((a.ca_address_sk == c.c_current_addr_sk))) continue; for (store_sales) |s| { if (!((c.c_customer_sk == s.ss_customer_sk))) continue; for (date_dim) |d| { if (!((s.ss_sold_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!((s.ss_item_sk == i.i_item_sk))) continue; if (!(((d.d_month_seq == target_month_seq) and (i.i_current_price > (1.2 * _avg_int(blk18: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (item) |j| { if (!((j.i_category == i.i_category))) continue; _tmp2.append(j.i_current_price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk18 _tmp3; })))))) continue; const _tmp6 = a.ca_state; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(a) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(a) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } } } } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("state", g.key) catch unreachable; m.put("cnt", (g.Items.len)) catch unreachable; break :blk17 m; }) catch unreachable; } break :blk19 _tmp7.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q6_result();
}
