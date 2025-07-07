const std = @import("std");

test "addition works" {
    const x = 1 + 2;
    try std.testing.expect(x == 3);
}

pub fn main() void {
    std.debug.print("ok\n", .{});
}
