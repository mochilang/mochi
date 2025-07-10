const std = @import("std");

const x = 5;

pub fn main() void {
    if ((x > 3)) {
        std.debug.print("big\n", .{});
    } else {
        std.debug.print("small\n", .{});
    }
}
