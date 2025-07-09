const std = @import("std");

var i = 0;

pub fn main() void {
    while ((i < 3)) {
        std.debug.print("{d}\n", .{i});
        i = (i + 1);
    }
}
