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

pub fn main() void {
    std.debug.print("{s}\n", .{label});
}
