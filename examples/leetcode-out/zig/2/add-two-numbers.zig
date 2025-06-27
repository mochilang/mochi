const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn addTwoNumbers(l1: []const i32, l2: []const i32) []const i32 {
    var i: i32 = @as(i32,@intCast(0));
    var j: i32 = @as(i32,@intCast(0));
    var carry: i32 = @as(i32,@intCast(0));
    var result = std.ArrayList(i32).init(std.heap.page_allocator);
    while ((((((i < (l1).len) or j) < (l2).len) or carry) > @as(i32,@intCast(0)))) {
        var x: i32 = @as(i32,@intCast(0));
        if ((i < (l1).len)) {
            x = l1[i];
            i = (i + @as(i32,@intCast(1)));
        }
        var y: i32 = @as(i32,@intCast(0));
        if ((j < (l2).len)) {
            y = l2[j];
            j = (j + @as(i32,@intCast(1)));
        }
        const sum: i32 = ((x + y) + carry);
        const digit: i32 = @mod(sum, @as(i32,@intCast(10)));
        carry = (sum / @as(i32,@intCast(10)));
        try result.append(@as(i32,@intCast(digit)));
    }
    return result.items;
}

fn test_example_1() void {
    expect((addTwoNumbers(&[_]i32{@as(i32,@intCast(2)), @as(i32,@intCast(4)), @as(i32,@intCast(3))}, &[_]i32{@as(i32,@intCast(5)), @as(i32,@intCast(6)), @as(i32,@intCast(4))}) == &[_]i32{@as(i32,@intCast(7)), @as(i32,@intCast(0)), @as(i32,@intCast(8))}));
}

fn test_example_2() void {
    expect((addTwoNumbers(&[_]i32{@as(i32,@intCast(0))}, &[_]i32{@as(i32,@intCast(0))}) == &[_]i32{@as(i32,@intCast(0))}));
}

fn test_example_3() void {
    expect((addTwoNumbers(&[_]i32{@as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9))}, &[_]i32{@as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9))}) == &[_]i32{@as(i32,@intCast(8)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(9)), @as(i32,@intCast(0)), @as(i32,@intCast(0)), @as(i32,@intCast(0)), @as(i32,@intCast(1))}));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
}
