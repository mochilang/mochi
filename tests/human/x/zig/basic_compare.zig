const std = @import("std");

pub fn main() void {
    const a: i32 = 10 - 3;
    const b: i32 = 2 + 2;
    std.debug.print("{d}\n", .{a});
    std.debug.print("{}\n", .{a == 7});
    std.debug.print("{}\n", .{b < 5});
}
