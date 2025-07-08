const std = @import("std");

var numbers: []const i32 = undefined;

pub fn main() void {
    numbers = &[_]i32{1, 2, 3, 4, 5, 6, 7, 8, 9};
    for (numbers) |n| {
        if ((@mod(n, 2) == 0)) {
            continue;
        }
        if ((n > 7)) {
            break;
        }
        std.debug.print("{s} {any}\n", .{"odd number:", n});
    }
}
