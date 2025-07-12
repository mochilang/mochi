const std = @import("std");

const x = 8; // i32
const msg = if ((x > 10)) ("big") else (if ((x > 5)) ("medium") else ("small")); // []const u8

pub fn main() void {
    std.debug.print("{s}\n", .{msg});
}
