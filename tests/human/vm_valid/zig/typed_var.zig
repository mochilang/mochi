const std = @import("std");

pub fn main() void {
    var x: i32 = 0; // zero-initialized
    std.debug.print("{d}\n", .{x});
}
