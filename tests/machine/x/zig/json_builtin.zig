const std = @import("std");

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

var m: std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    m = blk0: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("a", @as(i32, @intCast(1))) catch unreachable;
        m.put("b", @as(i32, @intCast(2))) catch unreachable;
        break :blk0 m;
    };
    _json(m);
}
