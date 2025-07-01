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

var sr_items: []const std.AutoHashMap([]const u8, i32) = undefined;
var cr_items: []const std.AutoHashMap([]const u8, i32) = undefined;
var wr_items: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q83_sample() void {
    expect((result == @as(i32,@intCast(83))));
}

pub fn main() void {
    sr_items = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(10))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(5))) catch unreachable; break :blk1 m; }};
    cr_items = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(25))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(20))) catch unreachable; break :blk3 m; }};
    wr_items = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(10))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(13))) catch unreachable; break :blk5 m; }};
    result = ((_sum_int(blk6: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (sr_items) |x| { _tmp0.append(x.qty) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk6 _tmp1; }) + _sum_int(blk7: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (cr_items) |x| { _tmp2.append(x.qty) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk7 _tmp3; })) + _sum_int(blk8: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (wr_items) |x| { _tmp4.append(x.qty) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk8 _tmp5; }));
    _json(result);
    test_TPCDS_Q83_sample();
}
