const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

var data: std.StringHashMap(struct { inner: i32, }) = undefined; // std.StringHashMap(struct { inner: i32, })

pub fn main() void {
    data["outer"]["inner"] = 2;
    std.debug.print("{any}\n", .{data["outer"]["inner"]});
}
