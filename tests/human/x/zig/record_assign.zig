const std = @import("std");

const Counter = struct { n: i32 };

fn inc(c: *Counter) void {
    c.n += 1;
}

pub fn main() void {
    var c = Counter{ .n = 0 };
    inc(&c);
    std.debug.print("{d}\n", .{c.n});
}
