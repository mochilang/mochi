const std = @import("std");

const Tree = union(enum) {
    Leaf,
    Node: struct { left: *Tree, value: i32, right: *Tree },
};

fn sumTree(t: *Tree) i32 {
    return switch (t.*) {
        .Leaf => 0,
        .Node => |n| sumTree(n.left) + n.value + sumTree(n.right),
    };
}

pub fn main() void {
    var leaf = Tree{ .Leaf = {} };
    var node_right = Tree{ .Node = .{ .left = &leaf, .value = 2, .right = &leaf } };
    var root = Tree{ .Node = .{ .left = &leaf, .value = 1, .right = &node_right } };
    std.debug.print("{d}\n", .{sumTree(&root)});
}
