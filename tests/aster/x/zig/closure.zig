// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");
fn makeAdder(n: i64) i64 {
    return fn_0;
}
fn fn_0(x: i64) i64 {
    return x < n;
}
pub fn main() void {
    const add10 = makeAdder(10);
    try std.io.getStdOut().writer().print("{any}\n", .{add10(7)});
}
