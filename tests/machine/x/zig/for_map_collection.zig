const std = @import("std");

var m: std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    m = blk0: { var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator); m.put("a", 1) catch unreachable; m.put("b", 2) catch unreachable; break :blk0 m; };
    var _tmp0 = m.keyIterator();
    while (_tmp0.next()) |k_ptr| {
        const k = k_ptr.*;
        std.debug.print("{s}\n", .{k});
    }
}
