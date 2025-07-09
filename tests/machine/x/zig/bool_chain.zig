const std = @import("std");

fn boom() bool {
    std.debug.print("{s}\n", .{"boom"});
    return true;
}

pub fn main() void {
    std.debug.print("{}\n", .{((((1 < 2)) and ((2 < 3))) and ((3 < 4)))});
    std.debug.print("{}\n", .{((((1 < 2)) and ((2 > 3))) and boom())});
    std.debug.print("{}\n", .{(((((1 < 2)) and ((2 < 3))) and ((3 > 4))) and boom())});
}
