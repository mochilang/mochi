const std = @import("std");

pub fn main() void {
    var map = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
    defer map.deinit();

    _ = map.put(1, "a");
    _ = map.put(2, "b");

    std.debug.print("{}\n", .{map.contains(1)});
    std.debug.print("{}\n", .{map.contains(3)});
}
