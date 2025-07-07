const std = @import("std");

const Todo = struct { title: []const u8 };

pub fn main() void {
    var todo = Todo{ .title = "hi" };
    std.debug.print("{s}\n", .{todo.title});
}
