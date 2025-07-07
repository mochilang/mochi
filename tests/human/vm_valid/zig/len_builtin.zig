const std = @import("std");

pub fn main() void {
    const arr = [_]i32{1,2,3};
    std.debug.print("{d}\n", .{arr.len});
}
