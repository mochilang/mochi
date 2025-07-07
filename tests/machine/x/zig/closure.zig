const std = @import("std");

var add10: fn(i32) i32 = undefined;

fn makeAdder(n: i32) i32 {
    return fn (x: i32) i32 {
        return (x + n);
};
}

pub fn main() void {
    add10 = makeAdder(@as(i32,@intCast(10)));
    std.debug.print("{any}\n", .{add10(@as(i32,@intCast(7)))});
}
