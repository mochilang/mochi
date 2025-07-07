const std = @import("std");

pub fn main() void {
    const y: i32 = 0; // default initialization
    std.debug.print("{d}\n", .{y});
}
