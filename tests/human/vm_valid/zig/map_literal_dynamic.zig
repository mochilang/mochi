const std = @import("std");

pub fn main() !void {
    var x: i32 = 3;
    var y: i32 = 4;
    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put("a", x);
    try map.put("b", y);

    std.debug.print("{d} {d}\n", .{map.get("a").?, map.get("b").?});
}
