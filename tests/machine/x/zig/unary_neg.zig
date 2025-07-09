const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{-3});
    std.debug.print("{d}\n", .{(5 + (-2))});
}
