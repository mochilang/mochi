const std = @import("std");

const m = struct {
    a: i32,
    b: i32,
    c: i32,
}{
    .a = 1,
    .b = 2,
    .c = 3,
};

pub fn main() void {
    std.debug.print("{any}\n", .{values(m)});
}
