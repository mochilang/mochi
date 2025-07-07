const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{blk0: { var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator); m.put("a", @as(i32,@intCast(1))) catch unreachable; m.put("b", @as(i32,@intCast(2))) catch unreachable; break :blk0 m; }.count()});
}
