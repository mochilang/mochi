const std = @import("std");

var x: i32 = undefined;
var y: i32 = undefined;
var m: std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    x = @as(i32, @intCast(3));
    y = @as(i32, @intCast(4));
    m = blk0: {
        var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
        m.put("a", x) catch unreachable;
        m.put("b", y) catch unreachable;
        break :blk0 m;
    };
    std.debug.print("{any} {any}\n", .{ m["a"], m["b"] });
}
