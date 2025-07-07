const std = @import("std");

pub fn main() void {
    const xs = [_]i32{10, 20, 30};
    std.debug.print("{d}\n", .{xs[1]});
}
