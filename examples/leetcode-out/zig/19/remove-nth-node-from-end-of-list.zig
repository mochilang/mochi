const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn removeNthFromEnd(nums: []const i32, n: i32) []const i32 {
    const idx: i32 = ((nums).len - n);
    var result = std.ArrayList(i32).init(std.heap.page_allocator);
    var i: i32 = @as(i32, @intCast(0));
    while ((i < (nums).len)) {
        if ((i != idx)) {
            try result.append(@as(i32, @intCast(nums[i])));
        }
        i = (i + @as(i32, @intCast(1)));
    }
    return result.items;
}

fn test_example_1() void {
    expect((removeNthFromEnd(&[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)), @as(i32, @intCast(4)), @as(i32, @intCast(5)) }, @as(i32, @intCast(2))) == &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)), @as(i32, @intCast(5)) }));
}

fn test_example_2() void {
    expect((removeNthFromEnd(&[_]i32{@as(i32, @intCast(1))}, @as(i32, @intCast(1))) == &[_]i32{}));
}

fn test_example_3() void {
    expect((removeNthFromEnd(&[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)) }, @as(i32, @intCast(1))) == &[_]i32{@as(i32, @intCast(1))}));
}

fn test_remove_first() void {
    expect((removeNthFromEnd(&[_]i32{ @as(i32, @intCast(7)), @as(i32, @intCast(8)), @as(i32, @intCast(9)) }, @as(i32, @intCast(3))) == &[_]i32{ @as(i32, @intCast(8)), @as(i32, @intCast(9)) }));
}

fn test_remove_last() void {
    expect((removeNthFromEnd(&[_]i32{ @as(i32, @intCast(7)), @as(i32, @intCast(8)), @as(i32, @intCast(9)) }, @as(i32, @intCast(1))) == &[_]i32{ @as(i32, @intCast(7)), @as(i32, @intCast(8)) }));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_remove_first();
    test_remove_last();
}
