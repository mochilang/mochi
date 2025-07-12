const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

var scores: std.StringHashMap(i32) = undefined; // std.StringHashMap(i32)

pub fn main() void {
    _ = scores.put("bob", 2) catch |err| handleError(err);
    std.debug.print("{d}\n", .{scores["bob"]});
}
