const std = @import("std");

const xs = &[_]i32{
    10,
    20,
    30,
}; // []const i32

pub fn main() void {
    std.debug.print("{d}\n", .{xs[1]});
}
