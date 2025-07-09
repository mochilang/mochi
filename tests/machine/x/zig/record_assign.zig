const std = @import("std");

const Counter = struct {
    n: i32,
};

var c = Counter{ .n = 0 };

fn inc(c: i32) void {
    c = (c.n + 1);
}

pub fn main() void {
    inc(c);
    std.debug.print("{d}\n", .{c.n});
}
