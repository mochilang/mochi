const std = @import("std");

var x: i32 = 0; // i32

pub fn main() void {
    std.debug.print("{d}\n", .{x});
}
