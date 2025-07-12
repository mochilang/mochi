const std = @import("std");

const r = 3.0; // f64
const area = (std.math.pi * std.math.pow(f64, r, 2.0)); // f64
const root = std.math.sqrt(49.0); // f64
const sin45 = std.math.sin((std.math.pi / 4.0)); // f64
const log_e = std.math.log(std.math.e); // f64

pub fn main() void {
    std.debug.print("Circle area with r = {any} => {any}\n", .{r, area});
    std.debug.print("Square root of 49: {any}\n", .{root});
    std.debug.print("sin(π/4): {any}\n", .{sin45});
    std.debug.print("log(e): {any}\n", .{log_e});
}
