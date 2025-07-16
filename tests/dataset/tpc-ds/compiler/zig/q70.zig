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

var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var dms: i32 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q70_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("s_state", "CA") catch unreachable; m.put("s_county", "Orange") catch unreachable; m.put("total_sum", 15) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("s_state", "TX") catch unreachable; m.put("s_county", "Travis") catch unreachable; m.put("total_sum", 20) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    store = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_state", "CA") catch unreachable; m.put("s_county", "Orange") catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(2))) catch unreachable; m.put("s_state", "CA") catch unreachable; m.put("s_county", "Orange") catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(3))) catch unreachable; m.put("s_state", "TX") catch unreachable; m.put("s_county", "Travis") catch unreachable; break :blk4 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1200))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1201))) catch unreachable; break :blk6 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_profit", 10) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_net_profit", 5) catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(3))) catch unreachable; m.put("ss_net_profit", 20) catch unreachable; break :blk9 m; }};
    dms = @as(i32,@intCast(1200));
    result = blk13: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; for (store) |s| { if (!((s.s_store_sk == ss.ss_store_sk))) continue; if (!(((d.d_month_seq >= dms) and (d.d_month_seq <= (dms + @as(i32,@intCast(11))))))) continue; const _tmp4 = blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("state", s.s_state) catch unreachable; m.put("county", s.s_county) catch unreachable; break :blk10 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_state", g.key.state) catch unreachable; m.put("s_county", g.key.county) catch unreachable; m.put("total_sum", _sum_int(blk12: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss.ss_net_profit) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk12 _tmp1; })) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk13 _tmp5.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q70_simplified();
}
