// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:21Z
const std = @import("std");

fn f() []const i32 {
    return [_]i32{
    0,
    0.0,
};
}

fn g(a: i32, b: f64) i32 {
    return 0;
}

fn h(s: []const u8, nums: []const i32) void {
}

fn user_main() void {
    f();
    g(1, 2.0);
    const res = f(); // []const i32
    g(res[0], res[1]);
    g(g(1, 2.0), 3.0);
}

pub fn main() void {
    user_main();
}
