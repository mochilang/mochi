const std = @import("std");

fn triple(x: i32) i32 {
    return (x * @as(i32, @intCast(3)));
}

pub fn main() void {
    std.debug.print("{any}\n", .{triple((@as(i32, @intCast(1)) + @as(i32, @intCast(2))))});
}
