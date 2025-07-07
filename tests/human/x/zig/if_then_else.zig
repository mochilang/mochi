const std = @import("std");

pub fn main() void {
    const x: i32 = 12;
    const msg = if (x > 10) "yes" else "no";
    std.debug.print("{}\n", .{msg});
}
