const std = @import("std");

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

const m = (blk0: { var _map0 = std.StringHashMap(i32).init(std.heap.page_allocator); _map0.put("a", 1) catch unreachable; _map0.put("b", 2) catch unreachable; break :blk0 _map0; });

pub fn main() void {
    _json(m);
}
