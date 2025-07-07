const std = @import("std");

fn boom() bool {
    std.debug.print("boom\n", .{});
    return true;
}

pub fn main() void {
    std.debug.print("{}\n", .{(1 < 2) && (2 < 3) && (3 < 4)});
    std.debug.print("{}\n", .{(1 < 2) && (2 > 3) && boom()});
    std.debug.print("{}\n", .{(1 < 2) && (2 < 3) && (3 > 4) && boom()});
}
