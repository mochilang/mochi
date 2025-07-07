const std = @import("std");

fn boom(a: i32, b: i32) bool {
    _ = a; _ = b;
    std.debug.print("boom\n", .{});
    return true;
}

pub fn main() void {
    std.debug.print("{}\n", .{false && boom(1, 2)});
    std.debug.print("{}\n", .{true || boom(1, 2)});
}
