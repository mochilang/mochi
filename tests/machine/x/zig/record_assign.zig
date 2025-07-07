const std = @import("std");

const Counter = struct {
    n: i32,
};

var c: i32 = undefined;

fn inc(c: i32) void {
    c = (c.n + @as(i32, @intCast(1)));
}

pub fn main() void {
    c = Counter{ .n = @as(i32, @intCast(0)) };
    inc(c);
    std.debug.print("{any}\n", .{c.n});
}
