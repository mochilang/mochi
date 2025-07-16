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

var sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var promotions: f64 = undefined;
var total: f64 = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q61_simplified() void {
    expect((result == @as(i32,@intCast(61))));
}

pub fn main() void {
    sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, bool).init(std.heap.page_allocator); m.put("promo", true) catch unreachable; m.put("price", @as(i32,@intCast(20))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, bool).init(std.heap.page_allocator); m.put("promo", true) catch unreachable; m.put("price", @as(i32,@intCast(41))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, bool).init(std.heap.page_allocator); m.put("promo", false) catch unreachable; m.put("price", @as(i32,@intCast(39))) catch unreachable; break :blk2 m; }};
    promotions = _sum_int(blk3: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (sales) |s| { if (!(s.promo)) continue; _tmp0.append(s.price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk3 _tmp1; });
    total = _sum_int(blk4: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (sales) |s| { _tmp2.append(s.price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk4 _tmp3; });
    result = ((promotions * @as(i32,@intCast(100))) / total);
    _json(result);
    test_TPCDS_Q61_simplified();
}
