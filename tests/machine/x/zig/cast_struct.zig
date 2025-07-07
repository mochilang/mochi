const std = @import("std");

const Todo = struct {
    title: []const u8,
};

var todo: i32 = undefined;

pub fn main() void {
    todo = @as(i32, blk0: {
        var m = std.AutoHashMap([]const u8, []const u8).init(std.heap.page_allocator);
        m.put("title", "hi") catch unreachable;
        break :blk0 m;
    });
    std.debug.print("{any}\n", .{todo.title});
}
