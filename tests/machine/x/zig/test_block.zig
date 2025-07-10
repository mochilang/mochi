const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn test_addition_works() void {
    const x = (1 + 2);
    expect((x == 3));
}

pub fn main() void {
    std.debug.print("ok\n", .{});
    test_addition_works();
}
