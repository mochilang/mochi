const std = @import("std");

fn inner(x: i32, y: i32) i32 {
    return x + y;
}

fn outer(x: i32) i32 {
    return inner(x, 5);
}

pub fn main() !void {
    std.debug.print("{}\n", .{outer(3)});
}
