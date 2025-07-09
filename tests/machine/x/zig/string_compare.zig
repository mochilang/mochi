const std = @import("std");

pub fn main() void {
    std.debug.print("{}\n", .{("a" < "b")});
    std.debug.print("{}\n", .{("a" <= "a")});
    std.debug.print("{}\n", .{("b" > "a")});
    std.debug.print("{}\n", .{("b" >= "b")});
}
