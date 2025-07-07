const std = @import("std");

fn boom() bool {
    std.debug.print("{s}\n", .{"boom"});
    return true;
}

pub fn main() void {
    std.debug.print("{any}\n", .{((((@as(i32,@intCast(1)) < @as(i32,@intCast(2)))) and ((@as(i32,@intCast(2)) < @as(i32,@intCast(3))))) and ((@as(i32,@intCast(3)) < @as(i32,@intCast(4)))))});
    std.debug.print("{any}\n", .{((((@as(i32,@intCast(1)) < @as(i32,@intCast(2)))) and ((@as(i32,@intCast(2)) > @as(i32,@intCast(3))))) and boom())});
    std.debug.print("{any}\n", .{(((((@as(i32,@intCast(1)) < @as(i32,@intCast(2)))) and ((@as(i32,@intCast(2)) < @as(i32,@intCast(3))))) and ((@as(i32,@intCast(3)) > @as(i32,@intCast(4))))) and boom())});
}
