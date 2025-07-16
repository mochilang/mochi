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

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q69_simplified() void {
    expect((result == @as(i32,@intCast(69))));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("amount", @as(i32,@intCast(34))) catch unreachable; break :blk0 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("amount", @as(i32,@intCast(35))) catch unreachable; break :blk1 m; }};
    result = (_sum_int(blk2: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |w| { _tmp0.append(w.amount) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk2 _tmp1; }) + _sum_int(blk3: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { _tmp2.append(s.amount) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk3 _tmp3; }));
    _json(result);
    test_TPCDS_Q69_simplified();
}
