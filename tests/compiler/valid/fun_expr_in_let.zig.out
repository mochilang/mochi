const std = @import("std");

var square: fn(i32) i32 = undefined;

pub fn main() void {
    square = fn (x: i32) i32 {
        return (x * x);
};
    std.debug.print("{any}\n", .{square(@as(i32,@intCast(6)))});
}
