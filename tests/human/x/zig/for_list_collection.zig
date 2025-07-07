const std = @import("std");

pub fn main() void {
    const items = [_]i32{1,2,3};
    for (items) |n| {
        std.debug.print("{d}\n", .{n});
    }
}
