const std = @import("std");

pub fn main() void {
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    _ = map.put("a", 1);
    _ = map.put("b", 2);

    std.debug.print("{d}\n", .{map.count()});
}
