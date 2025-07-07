const std = @import("std");

pub fn main() void {
    const x: i32 = 8;
    const msg = if (x > 10) "big" else if (x > 5) "medium" else "small";
    std.debug.print("{}\n", .{msg});
}
