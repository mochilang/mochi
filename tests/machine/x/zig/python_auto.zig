const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{std.math.sqrt(16.0)});
    std.debug.print("{any}\n", .{std.math.pi});
}
