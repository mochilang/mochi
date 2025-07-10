const std = @import("std");

var data: std.StringHashMap(struct { inner: i32, }) = undefined;

pub fn main() void {
    data["outer"]["inner"] = 2;
    std.debug.print("{any}\n", .{data["outer"]["inner"]});
}
