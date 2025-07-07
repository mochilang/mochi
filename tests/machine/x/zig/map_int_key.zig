const std = @import("std");

var m: std.AutoHashMap(i32, []const u8) = undefined;

pub fn main() void {
    m = blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put(@as(i32, @intCast(1)), "a") catch unreachable;
        m.put(@as(i32, @intCast(2)), "b") catch unreachable;
        break :blk0 m;
    };
    std.debug.print("{any}\n", .{m[@as(i32, @intCast(1))]});
}
