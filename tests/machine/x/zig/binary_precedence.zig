const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{(@as(i32,@intCast(1)) + (@as(i32,@intCast(2)) * @as(i32,@intCast(3))))});
    std.debug.print("{any}\n", .{(((@as(i32,@intCast(1)) + @as(i32,@intCast(2)))) * @as(i32,@intCast(3)))});
    std.debug.print("{any}\n", .{((@as(i32,@intCast(2)) * @as(i32,@intCast(3))) + @as(i32,@intCast(1)))});
    std.debug.print("{any}\n", .{(@as(i32,@intCast(2)) * ((@as(i32,@intCast(3)) + @as(i32,@intCast(1)))))});
}
