const std = @import("std");

fn outer(x: i32) i32 {
    const inner = (struct { x: i32, fn call(self: @This(), y: i32) i32 {
        return (self.x + y);
} }{ .x = x }).call;
    return inner(5);
}

pub fn main() void {
    std.debug.print("{d}\n", .{outer(3)});
}
