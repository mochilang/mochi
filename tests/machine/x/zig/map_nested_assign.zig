const std = @import("std");

var data: std.AutoHashMap([]const u8, std.AutoHashMap([]const u8, i32)) = undefined;

pub fn main() void {
    data = blk0: {
        var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
        m.put("outer", blk1: {
            var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
            m.put("inner", @as(i32, @intCast(1))) catch unreachable;
            break :blk1 m;
        }) catch unreachable;
        break :blk0 m;
    };
    data["outer"]["inner"] = @as(i32, @intCast(2));
    std.debug.print("{any}\n", .{data["outer"]["inner"]});
}
