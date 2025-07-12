const std = @import("std");

const x = 2; // i32
const label = switch (x) {1 => "one", 2 => "two", 3 => "three", else => "unknown", }; // []const u8
const day = "sun"; // []const u8
const mood = switch (day) {"mon" => "tired", "fri" => "excited", "sun" => "relaxed", else => "normal", }; // []const u8
const ok = true; // bool
const status = switch (ok) {true => "confirmed", false => "denied", }; // []const u8

fn classify(n: i32) []const u8 {
    return switch (n) {0 => "zero", 1 => "one", else => "many", };
}

pub fn main() void {
    std.debug.print("{s}\n", .{label});
    std.debug.print("{s}\n", .{mood});
    std.debug.print("{s}\n", .{status});
    std.debug.print("{s}\n", .{classify(0)});
    std.debug.print("{s}\n", .{classify(5)});
}
