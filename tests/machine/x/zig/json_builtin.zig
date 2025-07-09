const std = @import("std");

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

const m = struct { a: i32, b: i32, }{ .a = 1, .b = 2 };

pub fn main() void {
    _json(m);
}
