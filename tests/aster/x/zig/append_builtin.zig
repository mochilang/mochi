// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");
pub fn main() void {
    const a = [_]i64{1, 2};
    try std.io.getStdOut().writer().print("{any}\n", .{[_]i64{a[0], a[1], 3}});
}
