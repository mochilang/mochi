const std = @import("std");

const m = (blk0: { var _map0 = std.StringHashMap(i32).init(std.heap.page_allocator); _map0.put("a", 1) catch unreachable; _map0.put("b", 2) catch unreachable; break :blk0 _map0; });

pub fn main() void {
    std.debug.print("{any}\n", .{m["b"]});
}
