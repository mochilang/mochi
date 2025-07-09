const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{struct { a: i32, b: i32, }{ .a = 1, .b = 2 }.count()});
}
