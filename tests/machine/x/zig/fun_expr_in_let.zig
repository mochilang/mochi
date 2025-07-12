const std = @import("std");

const square = fn (x: i32) i32 {
        return (x * x);
}; // fn(i32) i32

pub fn main() void {
    std.debug.print("{d}\n", .{square(6)});
}
