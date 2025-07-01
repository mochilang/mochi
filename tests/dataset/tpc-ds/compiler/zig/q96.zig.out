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

const StoreSale = struct {
    ss_sold_time_sk: i32,
    ss_hdemo_sk: i32,
    ss_store_sk: i32,
};

const HouseholdDemographics = struct {
    hd_demo_sk: i32,
    hd_dep_count: i32,
};

const TimeDim = struct {
    t_time_sk: i32,
    t_hour: i32,
    t_minute: i32,
};

const Store = struct {
    s_store_sk: i32,
    s_store_name: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var time_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q96_count() void {
    expect((result == @as(i32,@intCast(3))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_time_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; break :blk2 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_dep_count", @as(i32,@intCast(3))) catch unreachable; break :blk3 m; }};
    time_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(1))) catch unreachable; m.put("t_hour", @as(i32,@intCast(20))) catch unreachable; m.put("t_minute", @as(i32,@intCast(35))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("t_time_sk", @as(i32,@intCast(2))) catch unreachable; m.put("t_hour", @as(i32,@intCast(20))) catch unreachable; m.put("t_minute", @as(i32,@intCast(45))) catch unreachable; break :blk5 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_store_name", "ese") catch unreachable; break :blk6 m; }};
    result = (blk7: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (household_demographics) |hd| { if (!((ss.ss_hdemo_sk == hd.hd_demo_sk))) continue; for (time_dim) |t| { if (!((ss.ss_sold_time_sk == t.t_time_sk))) continue; for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; if (!(((((t.t_hour == @as(i32,@intCast(20))) and (t.t_minute >= @as(i32,@intCast(30)))) and (hd.hd_dep_count == @as(i32,@intCast(3)))) and std.mem.eql(u8, s.s_store_name, "ese")))) continue; _tmp0.append(ss) catch unreachable; } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; }).len;
    _json(result);
    test_TPCDS_Q96_count();
}
