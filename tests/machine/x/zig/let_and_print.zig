const std = @import("std");

const a = 10;
const b = 20;

pub fn main() void {
    std.debug.print("{d}\n", .{(a + b)});
}
