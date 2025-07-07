const std = @import("std");

pub fn main() void {
    const arr = [_]i32{1,2,3};
    var total: i32 = 0;
    for (arr) |v| total += v;
    std.debug.print("{d}\n", .{total});
}
