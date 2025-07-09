const std = @import("std");

const square = fn (x: i32) i32 {
        return (x * x);
};

pub fn main() void {
    std.debug.print("{d}\n", .{square(6)});
}
