const std = @import("std");

var x: i32 = undefined;

pub fn main() void {
    x = @as(i32, @intCast(5));
    if ((x > @as(i32, @intCast(3)))) {
        std.debug.print("{s}\n", .{"big"});
    } else {
        std.debug.print("{s}\n", .{"small"});
    }
}
