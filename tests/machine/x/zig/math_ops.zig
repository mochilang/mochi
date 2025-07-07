const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{(@as(i32, @intCast(6)) * @as(i32, @intCast(7)))});
    std.debug.print("{any}\n", .{(@as(i32, @intCast(7)) / @as(i32, @intCast(2)))});
    std.debug.print("{any}\n", .{@mod(@as(i32, @intCast(7)), @as(i32, @intCast(2)))});
}
