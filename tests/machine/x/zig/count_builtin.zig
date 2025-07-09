const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{(&[_]i32{1, 2, 3}).len});
}
