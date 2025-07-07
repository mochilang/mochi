const std = @import("std");

var numbers: []const i32 = undefined;

pub fn main() void {
    numbers = &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)), @as(i32, @intCast(4)), @as(i32, @intCast(5)), @as(i32, @intCast(6)), @as(i32, @intCast(7)), @as(i32, @intCast(8)), @as(i32, @intCast(9)) };
    for (numbers) |n| {
        if ((@mod(n, @as(i32, @intCast(2))) == @as(i32, @intCast(0)))) {
            continue;
        }
        if ((n > @as(i32, @intCast(7)))) {
            break;
        }
        std.debug.print("{s} {any}\n", .{ "odd number:", n });
    }
}
