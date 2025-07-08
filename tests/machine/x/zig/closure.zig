const std = @import("std");

const add10 = makeAdder(10);
fn makeAdder(n: i32) fn(i32) i32 {
    return (struct { n: i32, fn call(self: @This(), x: i32) i32 {
        return (x + self.n);
} }{ .n = n }).call;
}

pub fn main() void {
    std.debug.print("{any}\n", .{add10(7)});
}
