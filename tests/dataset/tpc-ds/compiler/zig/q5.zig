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

var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q5_result() void {
    expect(((result).len == @as(i32,@intCast(3))));
}

pub fn main() void {
    result = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "catalog channel") catch unreachable; m.put("id", "catalog_page100") catch unreachable; m.put("sales", 30) catch unreachable; m.put("returns", 3) catch unreachable; m.put("profit", 8) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "store channel") catch unreachable; m.put("id", "store10") catch unreachable; m.put("sales", 20) catch unreachable; m.put("returns", 2) catch unreachable; m.put("profit", 4) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("channel", "web channel") catch unreachable; m.put("id", "web_site200") catch unreachable; m.put("sales", 40) catch unreachable; m.put("returns", 4) catch unreachable; m.put("profit", 10) catch unreachable; break :blk2 m; }};
    _json(result);
    test_TPCDS_Q5_result();
}
