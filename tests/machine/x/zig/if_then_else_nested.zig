const std = @import("std");

const x = 8;
const msg = if ((x > 10)) ("big") else (if ((x > 5)) ("medium") else ("small"));

pub fn main() void {
    std.debug.print("{s}\n", .{msg});
}
