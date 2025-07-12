const std = @import("std");

const Counter = struct { n: i32 };

var c = Counter{ .n = 0 }; // Counter

fn inc(c: *Counter) void {
    c.n = (c.n + 1);
}

pub fn main() void {
    inc(&c);
    std.debug.print("{d}\n", .{c.n});
}
