const std = @import("std");

pub fn main() !void {
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put("a", 1);
    try map.put("b", 2);
    try map.put("c", 3);

    var it = map.valueIterator();
    while (it.next()) |v| {
        std.debug.print("{d} ", .{v.*});
    }
    std.debug.print("\n", .{});
}
