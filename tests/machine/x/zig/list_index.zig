const std = @import("std");

var xs: []const i32 = undefined;

pub fn main() void {
    xs = &[_]i32{10, 20, 30};
    std.debug.print("{any}\n", .{xs[1]});
}
