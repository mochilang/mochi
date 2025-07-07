const std = @import("std");

pub fn main() !void {
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put("a", 1);
    try map.put("b", 2);

    std.debug.print("{}\n", .{map.contains("a")});
    std.debug.print("{}\n", .{map.contains("c")});
}
