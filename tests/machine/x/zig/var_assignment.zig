const std = @import("std");

var x = 1; // i32

pub fn main() void {
    x = 2;
    std.debug.print("{d}\n", .{x});
}
