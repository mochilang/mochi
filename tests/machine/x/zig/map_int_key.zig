const std = @import("std");

const m = (blk0: { var _map0 = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); _map0.put(1, "a") catch unreachable; _map0.put(2, "b") catch unreachable; break :blk0 _map0; });

pub fn main() void {
    std.debug.print("{s}\n", .{m[1]});
}
