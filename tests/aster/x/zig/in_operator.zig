// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");
pub fn main() void {
    const xs = [_]i64{1, 2, 3};
    try std.io.getStdOut().writer().print("{any}\n", .{std.mem.indexOfScalar(i64, xs, 2)});
    try std.io.getStdOut().writer().print("{any}\n", .{std.mem.indexOfScalar(i64, xs, 5)});
}
