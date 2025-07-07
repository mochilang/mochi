const std = @import("std");

pub fn main() void {
    var x: i32 = 1;
    x = 2;
    std.debug.print("{d}\n", .{x});
}
