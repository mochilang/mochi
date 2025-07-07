const std = @import("std");

fn boom(a: i32, b: i32) bool {
    std.debug.print("{s}\n", .{"boom"});
    return true;
}

pub fn main() void {
    std.debug.print("{any}\n", .{(false and boom(@as(i32, @intCast(1)), @as(i32, @intCast(2))))});
    std.debug.print("{any}\n", .{(true or boom(@as(i32, @intCast(1)), @as(i32, @intCast(2))))});
}
