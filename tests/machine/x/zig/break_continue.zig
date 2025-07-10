const std = @import("std");

const numbers = &[_]i32{
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
};

pub fn main() void {
    for (numbers) |n| {
        if ((@mod(n, 2) == 0)) {
            continue;
        }
        if ((n > 7)) {
            break;
        }
        std.debug.print("{s} {d}\n", .{"odd number:", n});
    }
}
