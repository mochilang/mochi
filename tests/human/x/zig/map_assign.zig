const std = @import("std");

pub fn main() !void {
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put("alice", 1);
    try map.put("bob", 2);
    const v = map.get("bob").?;
    std.debug.print("{d}\n", .{v});
}
