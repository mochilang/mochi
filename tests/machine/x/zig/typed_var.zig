const std = @import("std");

var x: i32 = 0;

pub fn main() void {
    std.debug.print("{d}\n", .{x});
}
