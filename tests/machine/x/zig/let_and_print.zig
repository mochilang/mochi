const std = @import("std");

const a = 10; // i32
const b = 20; // i32

pub fn main() void {
    std.debug.print("{d}\n", .{(a + b)});
}
