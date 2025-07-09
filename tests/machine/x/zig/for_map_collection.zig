const std = @import("std");

var m: std.StringHashMap(i32) = undefined;

pub fn main() void {
    m = (blk0: { var _tmp0 = std.StringHashMap(i32).init(std.heap.page_allocator); _tmp0.put("a", 1) catch unreachable; _tmp0.put("b", 2) catch unreachable; break :blk0 _tmp0; });
    var _tmp1 = m.keyIterator();
    while (_tmp1.next()) |k_ptr| {
        const k = k_ptr.*;
        std.debug.print("{s}\n", .{k});
    }
}
