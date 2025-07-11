const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{(2 + 3)});
    std.debug.print("{any}\n", .{3.14});
    std.debug.print("{any}\n", .{42});
}
