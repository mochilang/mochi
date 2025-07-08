const std = @import("std");

const Todo = struct {
    title: []const u8,
};

var todo: Todo = undefined;

pub fn main() void {
    todo = Todo{ .title = "hi" };
    std.debug.print("{s}\n", .{todo.title});
}
