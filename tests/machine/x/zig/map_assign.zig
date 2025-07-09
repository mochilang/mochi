const std = @import("std");

var scores: struct { alice: i32 } = undefined;

pub fn main() void {
    scores["bob"] = 2;
    std.debug.print("{any}\n", .{scores["bob"]});
}
