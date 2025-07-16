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

var web_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q85_sample() void {
    expect((result == 85));
}

pub fn main() void {
    web_returns = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(60))) catch unreachable; m.put("cash", 20) catch unreachable; m.put("fee", 1) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(100))) catch unreachable; m.put("cash", 30) catch unreachable; m.put("fee", 2) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", @as(i32,@intCast(95))) catch unreachable; m.put("cash", 25) catch unreachable; m.put("fee", 3) catch unreachable; break :blk2 m; }};
    result = _avg_int(blk3: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_returns) |r| { _tmp0.append(r.qty) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk3 _tmp1; });
    _json(result);
    test_TPCDS_Q85_sample();
}
