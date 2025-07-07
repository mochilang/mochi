const std = @import("std");

pub fn main() void {
    var numbers = [_]i32{1,2,3,4,5,6,7,8,9};
    for (numbers) |n| {
        if (n % 2 == 0) continue;
        if (n > 7) break;
        std.debug.print("odd number: {d}\n", .{n});
    }
}
