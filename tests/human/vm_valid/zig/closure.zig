const std = @import("std");

const Adder = struct {
    n: i32,
    pub fn apply(self: Adder, x: i32) i32 {
        return self.n + x;
    }
};

fn makeAdder(n: i32) Adder {
    return Adder{ .n = n };
}

pub fn main() void {
    var add10 = makeAdder(10);
    std.debug.print("{d}\n", .{add10.apply(7)});
}
