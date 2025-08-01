const std = @import("std");

fn classify(n: i32) []const u8 {
    return switch (n) {
        0 => "zero",
        1 => "one",
        else => "many",
    };
}

pub fn main() void {
    const x: i32 = 2;
    const label = switch (x) {
        1 => "one",
        2 => "two",
        3 => "three",
        else => "unknown",
    };
    std.debug.print("{s}\n", .{label});

    const day = "sun";
    const mood = switch (day) {
        "mon" => "tired",
        "fri" => "excited",
        "sun" => "relaxed",
        else => "normal",
    };
    std.debug.print("{s}\n", .{mood});

    const ok = true;
    const status = switch (ok) {
        true => "confirmed",
        false => "denied",
    };
    std.debug.print("{s}\n", .{status});

    std.debug.print("{s}\n", .{classify(0)});
    std.debug.print("{s}\n", .{classify(5)});
}
