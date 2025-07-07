const std = @import("std");

fn outer(x: i32) i32 {
    const inner = fn (y: i32) i32 {
        return (x + y);
};
    return inner(@as(i32,@intCast(5)));
}

pub fn main() void {
    std.debug.print("{any}\n", .{outer(@as(i32,@intCast(3)))});
}
