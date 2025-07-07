const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var x: i32 = undefined;
var label: []const u8 = undefined;
var day: []const u8 = undefined;
var mood: []const u8 = undefined;
var ok: bool = undefined;
var status: []const u8 = undefined;

fn classify(n: i32) []const u8 {
    return if (_equal(n, @as(i32, @intCast(0)))) "zero" else (if (_equal(n, @as(i32, @intCast(1)))) "one" else ("many"));
}

pub fn main() void {
    x = @as(i32, @intCast(2));
    label = if (_equal(x, @as(i32, @intCast(1)))) "one" else (if (_equal(x, @as(i32, @intCast(2)))) "two" else (if (_equal(x, @as(i32, @intCast(3)))) "three" else ("unknown")));
    std.debug.print("{s}\n", .{label});
    day = "sun";
    mood = if (_equal(day, "mon")) "tired" else (if (_equal(day, "fri")) "excited" else (if (_equal(day, "sun")) "relaxed" else ("normal")));
    std.debug.print("{s}\n", .{mood});
    ok = true;
    status = if (_equal(ok, true)) "confirmed" else (if (_equal(ok, false)) "denied" else (0));
    std.debug.print("{s}\n", .{status});
    std.debug.print("{any}\n", .{classify(@as(i32, @intCast(0)))});
    std.debug.print("{any}\n", .{classify(@as(i32, @intCast(5)))});
}
