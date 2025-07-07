const std = @import("std");

pub fn main() void {
    var i: i32 = 1;
    while (i < 4) : (i += 1) {
        std.debug.print("{d}\n", .{i});
    }
}
