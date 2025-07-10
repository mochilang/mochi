const std = @import("std");

const m = struct {
    a: i32,
    b: i32,
}{
    .a = 1,
    .b = 2,
};

pub fn main() void {
    std.debug.print("{any}\n", .{m["b"]});
}
