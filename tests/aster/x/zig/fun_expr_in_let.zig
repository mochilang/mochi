// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");
fn fn_0(x: i64) i64 {
    return x + x;
}
pub fn main() void {
    const square = fn_0;
    try std.io.getStdOut().writer().print("{any}\n", .{square(6)});
}
