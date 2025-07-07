const std = @import("std");

pub fn main() !void {
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put("a", 1);
    try map.put("b", 2);

    var it = map.keyIterator();
    while (it.next()) |key_ptr| {
        std.debug.print("{}\n", .{key_ptr.*});
    }
}
