const std = @import("std");

fn boom(a: i32, b: i32) bool {
    std.debug.print("boom\n", .{});
    return true;
}

pub fn main() void {
    std.debug.print("{}\n", .{(false and boom(1, 2))});
    std.debug.print("{}\n", .{(true or boom(1, 2))});
}
