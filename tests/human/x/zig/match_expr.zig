const std = @import("std");

pub fn main() void {
    const x: i32 = 2;
    const label = switch (x) {
        1 => "one",
        2 => "two",
        3 => "three",
        else => "unknown",
    };
    std.debug.print("{s}\n", .{label});
}
