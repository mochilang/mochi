const std = @import("std");

var data: struct { outer: struct { inner: i32, }, } = undefined;

pub fn main() void {
    data["outer"]["inner"] = 2;
    std.debug.print("{any}\n", .{data["outer"]["inner"]});
}
