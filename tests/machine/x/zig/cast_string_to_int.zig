const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

pub fn main() void {
    std.debug.print("{d}\n", .{std.fmt.parseInt(i32, "1995", 10) catch |err| handleError(err)});
}
