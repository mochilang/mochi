const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{@as(i32, "1995")});
}
