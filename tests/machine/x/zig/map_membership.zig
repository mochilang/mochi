const std = @import("std");

var m: std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    m = blk0: {
        var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
        m.put("a", @as(i32, @intCast(1))) catch unreachable;
        m.put("b", @as(i32, @intCast(2))) catch unreachable;
        break :blk0 m;
    };
    std.debug.print("{s}\n", .{m.contains("a")});
    std.debug.print("{s}\n", .{m.contains("c")});
}
