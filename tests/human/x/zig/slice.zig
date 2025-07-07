const std = @import("std");

pub fn main() void {
    const arr = [_]i32{1,2,3};
    std.debug.print("{any}\n", .{arr[1..3]});
    std.debug.print("{any}\n", .{arr[0..2]});
    const s = "hello";
    std.debug.print("{s}\n", .{s[1..4]});
}
