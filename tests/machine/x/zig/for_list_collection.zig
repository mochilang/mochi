const std = @import("std");

pub fn main() void {
    for (&[_]i32{1, 2, 3}) |n| {
        std.debug.print("{any}\n", .{n});
    }
}
