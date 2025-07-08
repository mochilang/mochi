const std = @import("std");

var x: i32 = undefined;

pub fn main() void {
    x = 5;
    if ((x > 3)) {
        std.debug.print("{s}\n", .{"big"});
    } else {
        std.debug.print("{s}\n", .{"small"});
    }
}
