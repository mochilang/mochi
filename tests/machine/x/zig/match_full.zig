const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const x = 2;
const label = if (_equal(x, 1)) "one" else (if (_equal(x, 2)) "two" else (if (_equal(x, 3)) "three" else ("unknown")));
const day = "sun";
const mood = if (_equal(day, "mon")) "tired" else (if (_equal(day, "fri")) "excited" else (if (_equal(day, "sun")) "relaxed" else ("normal")));
const ok = true;
const status = if (_equal(ok, true)) "confirmed" else (if (_equal(ok, false)) "denied" else (0));

fn classify(n: i32) []const u8 {
    return if (_equal(n, 0)) "zero" else (if (_equal(n, 1)) "one" else ("many"));
}

pub fn main() void {
    std.debug.print("{s}\n", .{label});
    std.debug.print("{s}\n", .{mood});
    std.debug.print("{s}\n", .{status});
    std.debug.print("{s}\n", .{classify(0)});
    std.debug.print("{s}\n", .{classify(5)});
}
