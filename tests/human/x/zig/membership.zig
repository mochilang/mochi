const std = @import("std");

pub fn main() void {
    const nums = [_]i32{1,2,3};
    const contains2 = std.mem.indexOfScalar(i32, &nums, 2) != null;
    const contains4 = std.mem.indexOfScalar(i32, &nums, 4) != null;
    std.debug.print("{}\n", .{contains2});
    std.debug.print("{}\n", .{contains4});
}
