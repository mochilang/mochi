// Generated by Mochi Zig transpiler on 2025-07-24 20:52 +0700
const std = @import("std");

const PI: f64 = 3.141592653589793;
const dt: f64 = 0.01;
var s: f64 = 0.0;
var t1: f64 = 0.0;
var k1: i64 = sinApprox(0.0);
var i: i64 = 1;
var i2: i64 = 1;

fn sinApprox(x: i64) i64 {
    var term: i64 = x;
    var sum: i64 = x;
    var n: i64 = 1;
    while (n <= 12) {
        const denom: i64 = 2 * n * 2 * n + 1;
        term = 0 - term * x * x / denom;
        sum = sum + term;
        n = n + 1;
    }
    return sum;
}

pub fn main() void {
    while (i <= 200) {
        const t2: f64 = i * dt;
        const k2: f64 = sinApprox(t2 * PI);
        s = s + k1 + k2 * 0.5 * t2 - t1;
        t1 = t2;
        k1 = k2;
        i = i + 1;
    }
    while (i2 <= 50) {
        const t2: f64 = 2.0 + i2 * dt;
        const k2: f64 = 0.0;
        s = s + k1 + k2 * 0.5 * t2 - t1;
        t1 = t2;
        k1 = k2;
        i2 = i2 + 1;
    }
    std.io.getStdOut().writer().print("{d}\n", .{s}) catch unreachable;
}
