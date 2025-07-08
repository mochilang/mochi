const std = @import("std");

const square = struct { fn call(x: i32) i32 {
    return (x * x);
} }.call;

pub fn main() void {
    std.debug.print("{any}\n", .{square(6)});
}
