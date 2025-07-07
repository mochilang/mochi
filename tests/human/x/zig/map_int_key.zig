const std = @import("std");

pub fn main() !void {
    var map = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put(1, "a");
    try map.put(2, "b");

    const val = map.get(1).?;
    std.debug.print("{s}\n", .{val});
}
