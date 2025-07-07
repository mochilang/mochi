const std = @import("std");

pub fn main() void {
    std.debug.print("{s}\n", .{("a" < "b")});
    std.debug.print("{s}\n", .{("a" <= "a")});
    std.debug.print("{s}\n", .{("b" > "a")});
    std.debug.print("{s}\n", .{("b" >= "b")});
}
