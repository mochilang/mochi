const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{42});
    std.debug.print("{any}\n", .{3});
    std.debug.print("{d}\n", .{1});
}
