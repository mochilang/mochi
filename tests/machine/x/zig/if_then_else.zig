const std = @import("std");

const x = 12;
const msg = if ((x > 10)) ("yes") else ("no");

pub fn main() void {
    std.debug.print("{s}\n", .{msg});
}
