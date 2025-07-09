const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{(6 * 7)});
    std.debug.print("{d}\n", .{(7 / 2)});
    std.debug.print("{d}\n", .{@mod(7, 2)});
}
