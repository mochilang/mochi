const std = @import("std");

pub fn main() !void {
    var inner = std.StringHashMap(i32).init(std.heap.page_allocator);
    try inner.put("inner", 1);

    var outer = std.StringHashMap(*std.StringHashMap(i32)).init(std.heap.page_allocator);
    try outer.put("outer", &inner);

    // modify nested value
    outer.get("outer").?.put("inner", 2) catch unreachable;

    std.debug.print("{d}\n", .{outer.get("outer").?.get("inner").?});

    inner.deinit();
    outer.deinit();
}
