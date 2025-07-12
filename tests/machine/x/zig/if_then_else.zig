const std = @import("std");

const x = 12; // i32
const msg = if ((x > 10)) ("yes") else ("no"); // []const u8

pub fn main() void {
    std.debug.print("{s}\n", .{msg});
}
