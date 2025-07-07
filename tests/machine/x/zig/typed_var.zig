const std = @import("std");

var x: i32 = undefined;

pub fn main() void {
    x = 0;
    std.debug.print("{any}\n", .{x});
}
