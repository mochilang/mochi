const std = @import("std");

pub fn main() !void {
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put("a", 1);
    try map.put("b", 2);

    const val = map.get("b").?; // safe since key exists
    std.debug.print("{d}\n", .{val});
}
