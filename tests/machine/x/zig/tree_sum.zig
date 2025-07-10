const std = @import("std");

const Tree = union(enum) {
    Leaf,
    Node: struct { left: *Tree, value: i32, right: *Tree },
};

const t = Tree{ .Node = .{ .left = &Tree{ .Leaf = {} }, .value = 1, .right = &Tree{ .Node = .{ .left = &Tree{ .Leaf = {} }, .value = 2, .right = &Tree{ .Leaf = {} } } } } };

fn sum_tree(t: *Tree) i32 {
    return switch (t.*) {.Leaf => 0, .Node => |_tmp0| ((sum_tree(&_tmp0.left) + _tmp0.value) + sum_tree(&_tmp0.right)), };
}

pub fn main() void {
    std.debug.print("{d}\n", .{sum_tree(&t)});
}
