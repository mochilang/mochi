const std = @import("std");

pub fn main() void {
    const data = [_]i32{1, 2};
    var flag = false;
    for (data) |x| {
        if (x == 1) {
            flag = true;
            break;
        }
    }
    std.debug.print("{}\n", .{flag});
}
