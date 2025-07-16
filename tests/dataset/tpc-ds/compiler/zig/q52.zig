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

var t: []const std.AutoHashMap([]const u8, i32) = undefined;
var tmp: []const u8 = undefined;
var vals: []const i32 = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q52_placeholder() void {
    expect((result == @as(i32,@intCast(52))));
}

pub fn main() void {
    t = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", @as(i32,@intCast(1))) catch unreachable; m.put("val", @as(i32,@intCast(52))) catch unreachable; break :blk0 m; }};
    tmp = std.ascii.lowerString("ignore");
    vals = blk1: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (t) |r| { _tmp0.append(r.val) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk1 _tmp1; };
    result = first(vals);
    _json(result);
    test_TPCDS_Q52_placeholder();
}
