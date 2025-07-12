const std = @import("std");

const x = 2; // i32
const label = switch (x) {1 => "one", 2 => "two", 3 => "three", else => "unknown", }; // []const u8

pub fn main() void {
    std.debug.print("{s}\n", .{label});
}
