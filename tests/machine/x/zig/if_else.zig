const std = @import("std");

const x = 5;

pub fn main() void {
    if ((x > 3)) {
        std.debug.print("{s}\n", .{"big"});
    } else {
        std.debug.print("{s}\n", .{"small"});
    }
}
