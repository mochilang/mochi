const std = @import("std");

var y: i32 = 0;

pub fn main() void {
    std.debug.print("{d}\n", .{y});
}
