const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn reverse(x: i32) i32 {
    var sign: i32 = @as(i32,@intCast(1));
    var n: i32 = x;
    if ((n < @as(i32,@intCast(0)))) {
        sign = -@as(i32,@intCast(1));
        n = -n;
    }
    var rev: i32 = @as(i32,@intCast(0));
    while ((n != @as(i32,@intCast(0)))) {
        const digit: i32 = @mod(n, @as(i32,@intCast(10)));
        rev = ((rev * @as(i32,@intCast(10))) + digit);
        n = (n / @as(i32,@intCast(10)));
    }
    rev = (rev * sign);
    if ((((rev < ((-@as(i32,@intCast(2147483647)) - @as(i32,@intCast(1))))) or rev) > @as(i32,@intCast(2147483647)))) {
        return @as(i32,@intCast(0));
    }
    return rev;
}

fn test_example_1() void {
    expect((reverse(@as(i32,@intCast(123))) == @as(i32,@intCast(321))));
}

fn test_example_2() void {
    expect((reverse(-@as(i32,@intCast(123))) == (-@as(i32,@intCast(321)))));
}

fn test_example_3() void {
    expect((reverse(@as(i32,@intCast(120))) == @as(i32,@intCast(21))));
}

fn test_overflow() void {
    expect((reverse(@as(i32,@intCast(1534236469))) == @as(i32,@intCast(0))));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_overflow();
}
